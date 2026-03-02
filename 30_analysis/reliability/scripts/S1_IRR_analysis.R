################################################################################
# Inter-Rater Reliability (China stratified 25-university subsample)
# Compare: 25-university coder file vs 128-university coding results file
# Zhicheng Lin
# Last Updated: Jan 21, 2026
################################################################################

# ==============================================================================
# 1. PACKAGES & SETUP
# ==============================================================================
package_list <- c("tidyverse", "irr", "knitr", "rstudioapi")

load_packages <- function(pkgs) {
  new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
  if (length(new_pkgs)) install.packages(new_pkgs)
  invisible(lapply(pkgs, library, character.only = TRUE))
}
load_packages(package_list)

# Resolve script path so IO works regardless of working directory.
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (rstudioapi::isAvailable()) {
    return(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
  return(normalizePath("."))
}

script_dir <- get_script_dir()
input_dir <- normalizePath(file.path(script_dir, "../../../20_coding/23_results"), mustWork = TRUE)
output_dir <- normalizePath(file.path(script_dir, "../outputs"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 2. DATA IMPORT
# ==============================================================================
china_full <- read_csv(file.path(input_dir, "S1_china_irr_base.csv"), show_col_types = FALSE) %>%
  # Normalize names like "linked_to_pass(0/1)" -> "linked_to_pass"
  rename_with(~ stringr::str_replace_all(.x, "\\(0/1\\)", ""))

china_25 <- read_csv(file.path(input_dir, "S1_China_25_Results_Code.csv"), show_col_types = FALSE) %>%
  rename_with(~ stringr::str_replace_all(.x, "\\(0/1\\)", ""))

# ==============================================================================
# 3. CHECK STRATIFIED SAMPLE (N=25)
# ==============================================================================
# The target stratified quotas:
# 综合 9; 理工 8; 师范 2; 医药/财经/农业/语言/中医药/民族 each 1  (total = 25)
target_quota <- tibble(
  Inst_type = c("综合","理工","师范","医药","财经","农业","语言","中医药","民族"),
  target_n  = c(9, 8, 2, 1, 1, 1, 1, 1, 1)
)

quota_check <- china_25 %>%
  count(Inst_type, name = "observed_n") %>%
  right_join(target_quota, by = "Inst_type") %>%
  mutate(
    observed_n = replace_na(observed_n, 0),
    ok = (observed_n == target_n)
  ) %>%
  arrange(match(Inst_type, target_quota$Inst_type))

cat("\n--- Stratified sample quota check (should all be ok=TRUE) ---\n")
print(knitr::kable(quota_check, format = "simple"))

# Ensure the 25 IDs are all present in the 128 file
common_ids <- intersect(china_full$id, china_25$id)
if (length(common_ids) != nrow(china_25)) {
  stop("ERROR: Some IDs in S1_China_25_Results_Code.csv are not found in S1_china_irr_base.csv")
}

# ==============================================================================
# 4. ALIGN CODER A vs CODER B CODES
# ==============================================================================
vars <- c("check_framework", "tool_specified", "linked_to_pass")

# Sanity check: required columns exist
missing_full <- setdiff(vars, names(china_full))
missing_25   <- setdiff(vars, names(china_25))
if (length(missing_full) > 0) stop("Missing in full file: ", paste(missing_full, collapse = ", "))
if (length(missing_25) > 0)   stop("Missing in 25 file: ", paste(missing_25, collapse = ", "))

# Treat full file as "Coder A / Final codes" for the 25 IDs
coderA <- china_full %>%
  select(id, all_of(vars)) %>%
  rename_with(~ paste0("A_", .x), all_of(vars)) %>%
  mutate(across(starts_with("A_"), ~ as.integer(.x)))

# Treat the 25-university file as "Coder B"
coderB <- china_25 %>%
  select(id, univ_name_cn, univ_name_en, shanghairank_2025, Inst_type, all_of(vars)) %>%
  rename_with(~ paste0("B_", .x), all_of(vars)) %>%
  mutate(across(starts_with("B_"), ~ as.integer(.x)))

irr_df <- coderB %>%
  inner_join(coderA, by = "id") %>%
  arrange(id)

# ==============================================================================
# 5. AGREEMENT FUNCTIONS (BINARY 0/1)
# ==============================================================================

# ---- Gwet's AC1 (binary, 2 raters) ----
# Uses p_i = average marginal proportion across the two raters.
# For k=2 categories, Pe = 2*p0*p1, AC1 = (Po - Pe) / (1 - Pe).
gwet_ac1_binary <- function(a, b) {
  keep <- !(is.na(a) | is.na(b))
  a <- a[keep]; b <- b[keep]
  n <- length(a)
  if (n == 0) return(NA_real_)
  
  Po <- mean(a == b)
  
  pA1 <- mean(a == 1)
  pB1 <- mean(b == 1)
  p1  <- (pA1 + pB1) / 2
  p0  <- 1 - p1
  
  Pe  <- 2 * p0 * p1
  
  # If Pe is 1 (pathological), AC1 is not estimable; otherwise compute normally
  if (abs(1 - Pe) < 1e-12) return(NA_real_)
  (Po - Pe) / (1 - Pe)
}

calculate_agreement <- function(df, var_name) {
  
  col_a <- paste0("A_", var_name)
  col_b <- paste0("B_", var_name)
  
  ratings <- df %>%
    select(all_of(c(col_a, col_b))) %>%
    drop_na()
  
  n <- nrow(ratings)
  agree_n <- sum(ratings[[1]] == ratings[[2]])
  po <- agree_n / n
  pct_agree <- po * 100
  
  # 2x2 table (force levels 0/1)
  tbl <- table(
    factor(ratings[[1]], levels = c(0, 1)),
    factor(ratings[[2]], levels = c(0, 1))
  )
  
  n00 <- tbl[1, 1]; n01 <- tbl[1, 2]
  n10 <- tbl[2, 1]; n11 <- tbl[2, 2]
  
  # Cohen's Kappa
  k_res <- tryCatch(
    irr::kappa2(ratings, weight = "unweighted"),
    error = function(e) NULL
  )
  kappa_val <- if (is.null(k_res)) NA_real_ else k_res$value
  kappa_p   <- if (is.null(k_res)) NA_real_ else k_res$p.value
  
  # ---- Gwet's AC1 ----
  ac1_val <- gwet_ac1_binary(ratings[[1]], ratings[[2]])
  
  # PABAK
  pabak <- 2 * po - 1
  
  # Diagnostics
  # Standard prevalence & bias indices for 2x2 tables (Byrt et al.)
  prevalence_index <- abs(n11 - n00) / n
  bias_index <- abs(n01 - n10) / n
  
  # Your prior "prevalence-like" diagnostic (actually |2*Po - 1|)
  agreement_index_abs <- abs((n00 + n11) - (n01 + n10)) / n
  
  # Positive/Negative agreement
  pos_agree <- if ((2*n11 + n01 + n10) == 0) NA_real_ else (2*n11) / (2*n11 + n01 + n10)
  neg_agree <- if ((2*n00 + n01 + n10) == 0) NA_real_ else (2*n00) / (2*n00 + n01 + n10)
  
  tibble(
    Variable = var_name,
    N = n,
    Pct_Agreement = pct_agree,
    Kappa = kappa_val,
    Kappa_P = kappa_p,
    AC1 = ac1_val,
    PABAK = pabak,
    Prevalence_Index = prevalence_index,
    Bias_Index = bias_index,
    Agreement_Index_abs = agreement_index_abs,
    Positive_Agreement = pos_agree,
    Negative_Agreement = neg_agree,
    Disagreements = n - agree_n,
    A_1s = sum(ratings[[1]] == 1),
    B_1s = sum(ratings[[2]] == 1)
  )
}

# ==============================================================================
# 6. RUN IRR
# ==============================================================================
results_table <- purrr::map_dfr(vars, ~ calculate_agreement(irr_df, .x)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

cat("\n--- IRR Summary (China N=25 overlap) ---\n")
print(knitr::kable(results_table, digits = 4, format = "simple"))

# ==============================================================================
# 7. DISAGREEMENTS (FOR MANUAL REVIEW)
# ==============================================================================
disagreements <- purrr::map_dfr(vars, function(v) {
  a <- paste0("A_", v)
  b <- paste0("B_", v)
  
  irr_df %>%
    filter(.data[[a]] != .data[[b]]) %>%
    transmute(
      id, univ_name_cn, univ_name_en, shanghairank_2025, Inst_type,
      Variable = v,
      Coder_A = .data[[a]],
      Coder_B = .data[[b]]
    )
})

cat("\n--- Disagreements (if any) ---\n")
print(knitr::kable(disagreements, format = "simple"))

# ==============================================================================
# 8. EXPORT
# ==============================================================================
write_csv(results_table,  file.path(output_dir, "S1_IRR_Summary_25.csv"))
write_csv(disagreements,  file.path(output_dir, "S1_IRR_Disagreements_25.csv"))
write_csv(irr_df,         file.path(output_dir, "S1_IRR_Aligned_25.csv"))

cat("\nAnalysis complete. Outputs saved:\n",
    " - ", file.path(output_dir, "S1_IRR_Summary_25.csv"), "\n",
    " - ", file.path(output_dir, "S1_IRR_Disagreements_25.csv"), "\n",
    " - ", file.path(output_dir, "S1_IRR_Aligned_25.csv"), "\n")
