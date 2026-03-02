################################################################################
# Inter-Rater Reliability Analysis
# Zhicheng Lin
# Last Updated: Jan 21, 2026
################################################################################

# ==============================================================================
# 1. PACKAGES & SETUP
# ==============================================================================
package_list <- c("tidyverse", "irr", "irrCAC", "knitr")

load_packages <- function(pkgs) {
  new_pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
  if(length(new_pkgs)) install.packages(new_pkgs)
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
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
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
data_raw <- read_csv(file.path(input_dir, "S2_international_irr.csv"), show_col_types = FALSE)

# ==============================================================================
# 3. ANALYSIS FUNCTION
# ==============================================================================
calculate_agreement <- function(data, dim_name) {
  
  col_a <- paste0("A_", dim_name)
  col_b <- paste0("B_", dim_name)
  
  ratings <- data %>%
    select(all_of(c(col_a, col_b))) %>%
    drop_na()
  
  n <- nrow(ratings)
  A <- ratings[[1]]
  B <- ratings[[2]]
  
  # Confusion matrix with fixed levels
  tbl <- table(factor(A, levels = 0:1), factor(B, levels = 0:1))
  a <- tbl[1,1]; b <- tbl[1,2]
  c <- tbl[2,1]; d <- tbl[2,2]
  
  # Observed agreement
  po <- (a + d) / n
  pct_agree <- 100 * po
  
  # Cohen's kappa (keep NA if undefined)
  k_res <- kappa2(ratings)
  kappa_val <- k_res$value
  kappa_p   <- k_res$p.value
  
  # Byrt et al. prevalence & bias indices
  prev_index <- abs(a - d) / n
  bias_index <- abs(b - c) / n
  
  # PABAK (descriptive; essentially 2*Po - 1)
  pabak <- 2 * po - 1
  
  # Positive/negative agreement (helpful under skew)
  ppos <- if ((2*d + b + c) == 0) NA else (2*d) / (2*d + b + c)
  pneg <- if ((2*a + b + c) == 0) NA else (2*a) / (2*a + b + c)
  
  # Gwet's AC1 (recommended primary chance-corrected stat here)
  ac1 <- irrCAC::gwet.ac1.raw(as.matrix(ratings))$est$coeff.val
  
  tibble(
    Dimension = dim_name,
    N = n,
    Pct_Agreement = pct_agree,
    Kappa = kappa_val,
    Kappa_P = kappa_p,
    AC1 = ac1,
    PABAK = pabak,
    Pos_Agree = ppos,
    Neg_Agree = pneg,
    Prevalence_Index = prev_index,
    Bias_Index = bias_index,
    Disagreements = (b + c),
    Note = ifelse(is.na(kappa_val), "κ not estimable (constant ratings / no variance)", "")
  )
}

# ==============================================================================
# 4. EXECUTION
# ==============================================================================
dimensions <- c("D1", "D2", "D3", "D4")

results_table <- purrr::map_dfr(dimensions, ~calculate_agreement(data_raw, .x)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

print(knitr::kable(results_table, format = "simple"))

# Disagreements for audit
disagreements_list <- purrr::map_dfr(dimensions, function(d) {
  col_a <- paste0("A_", d)
  col_b <- paste0("B_", d)
  
  data_raw %>%
    filter(.data[[col_a]] != .data[[col_b]]) %>%
    select(id, univ_name, rank_usnews_2025, country_region,
           Rater_A = all_of(col_a), Rater_B = all_of(col_b)) %>%
    mutate(Dimension = d)
})

# ==============================================================================
# 5. EXPORT
# ==============================================================================
write_csv(results_table, file.path(output_dir, "S2_IRR_Summary_Table.csv"))
write_csv(disagreements_list, file.path(output_dir, "S2_IRR_Disagreements_Review.csv"))
cat("\nAnalysis Complete. Files saved.\n")
