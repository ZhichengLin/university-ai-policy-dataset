################################################################################
# F3 Analysis: Alternative scoring and sensitivity checks
# Produces manuscript-facing outputs for Supplemental Materials F3
################################################################################

resolve_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
  normalizePath(".")
}

script_dir <- resolve_script_dir()
root_dir <- normalizePath(file.path(script_dir, "../../.."), mustWork = TRUE)
input_file <- normalizePath(file.path(root_dir, "20_coding/21_store/S1_china_analysis.csv"), mustWork = TRUE)
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

china <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

china$id_chr <- as.character(china$id)
china$check_framework <- to_num(china[["check_framework(0/1)"]])
china$tool_specified <- to_num(china[["tool_specified(0/1)"]])
china$linked_to_pass <- to_num(china[["linked_to_pass(0/1)"]])
china$rank_numeric <- to_num(china[["shanghairank_2025"]])

ordinal_index <- function(cf, ts, lp) {
  idx <- rep(NA_real_, length(cf))
  idx[cf == 0] <- 0
  idx[cf == 1 & ts == 0 & lp == 0] <- 1
  idx[cf == 1 & ((ts == 1 & lp == 0) | (ts == 0 & lp == 1))] <- 2
  idx[cf == 1 & ts == 1 & lp == 1] <- 3
  idx
}

compute_scenario_stats <- function(df, scenario_name) {
  top100 <- subset(df, !is.na(rank_numeric) & rank_numeric <= 100)
  top100$ord <- ordinal_index(top100$check_framework, top100$tool_specified, top100$linked_to_pass)

  # Rank association checks requested by supplement text
  spearman <- suppressWarnings(cor.test(top100$ord, top100$rank_numeric, method = "spearman"))
  pointbis <- suppressWarnings(cor.test(top100$check_framework, top100$rank_numeric, method = "pearson"))

  n_adopters <- sum(top100$check_framework == 1, na.rm = TRUE)
  n_linked <- sum(top100$linked_to_pass == 1, na.rm = TRUE)
  n_full <- sum(top100$check_framework == 1 & top100$tool_specified == 1 & top100$linked_to_pass == 1, na.rm = TRUE)

  data.frame(
    scenario = scenario_name,
    n_top100_numeric = nrow(top100),
    n_adopters = n_adopters,
    n_linked_to_pass = n_linked,
    n_full_stack = n_full,
    spearman_rho_ord_rank = unname(spearman$estimate),
    spearman_p = spearman$p.value,
    pointbiserial_r_cf_rank = unname(pointbis$estimate),
    pointbiserial_p = pointbis$p.value
  )
}

extract_glm <- function(model_obj, scenario_name, model_name) {
  sm <- summary(model_obj)
  coef_mat <- as.data.frame(sm$coefficients, stringsAsFactors = FALSE)
  coef_mat$term <- rownames(coef_mat)
  rownames(coef_mat) <- NULL
  names(coef_mat) <- c("estimate", "std_error", "z_value", "p_value", "term")
  coef_mat$scenario <- scenario_name
  coef_mat$model <- model_name
  coef_mat <- coef_mat[, c("scenario", "model", "term", "estimate", "std_error", "z_value", "p_value")]

  diag_row <- data.frame(
    scenario = scenario_name,
    model = model_name,
    converged = isTRUE(model_obj$converged),
    n = stats::nobs(model_obj),
    aic = AIC(model_obj)
  )

  list(coef = coef_mat, diag = diag_row)
}

run_glm_safe <- function(df, scenario_name, model_name, formula_obj) {
  out <- tryCatch(
    {
      fit <- glm(formula_obj, data = df, family = binomial())
      extract_glm(fit, scenario_name, model_name)
    },
    error = function(e) {
      list(
        coef = data.frame(
          scenario = scenario_name,
          model = model_name,
          term = NA_character_,
          estimate = NA_real_,
          std_error = NA_real_,
          z_value = NA_real_,
          p_value = NA_real_
        ),
        diag = data.frame(
          scenario = scenario_name,
          model = model_name,
          converged = FALSE,
          n = nrow(df),
          aic = NA_real_
        )
      )
    }
  )
  out
}

# Identify potentially ambiguous linkage language for boundary recoding
text_blob <- paste(
  ifelse(is.na(china$policy_summary), "", china$policy_summary),
  ifelse(is.na(china$threshold), "", china$threshold),
  ifelse(is.na(china$Rationale), "", china$Rationale)
)

ambiguous_pattern <- paste(
  c(
    "in principle",
    "for reference only",
    "important reference",
    "recommended that",
    "it is recommended",
    "may include delayed defense",
    "may be regarded",
    "auxiliary reference"
  ),
  collapse = "|"
)

ambiguous_flag <- grepl(ambiguous_pattern, text_blob, ignore.case = TRUE) & china$check_framework == 1
ambiguous_cases <- china[ambiguous_flag, c("id", "univ_name_cn", "univ_name_en", "Inst_type", "linked_to_pass", "tool_specified")]
ambiguous_cases$boundary_flag_reason <- "pattern_match_in_policy_summary_or_threshold"

# Baseline + boundary scenarios
baseline <- china
conservative <- china
conservative$linked_to_pass[ambiguous_flag] <- 0
expansive <- china
expansive$linked_to_pass[ambiguous_flag] <- 1

scenario_stats <- rbind(
  compute_scenario_stats(baseline, "baseline"),
  compute_scenario_stats(conservative, "conservative"),
  compute_scenario_stats(expansive, "expansive")
)

# Logistic models with institutional type covariates (top100 numeric subset)
build_model_data <- function(df) {
  m <- subset(df, !is.na(rank_numeric) & rank_numeric <= 100)
  m$ord <- ordinal_index(m$check_framework, m$tool_specified, m$linked_to_pass)
  m$high_enforcement <- ifelse(m$ord >= 2, 1, 0)
  m$rank_z <- as.numeric(scale(m$rank_numeric))
  m$Inst_type <- as.factor(m$Inst_type)
  m
}

coef_out <- data.frame()
diag_out <- data.frame()

for (scenario_name in c("baseline", "conservative", "expansive")) {
  dat <- switch(
    scenario_name,
    baseline = build_model_data(baseline),
    conservative = build_model_data(conservative),
    expansive = build_model_data(expansive)
  )

  m1 <- run_glm_safe(dat, scenario_name, "check_framework ~ rank_z + Inst_type", check_framework ~ rank_z + Inst_type)
  m2 <- run_glm_safe(dat, scenario_name, "high_enforcement ~ rank_z + Inst_type", high_enforcement ~ rank_z + Inst_type)

  coef_out <- rbind(coef_out, m1$coef, m2$coef)
  diag_out <- rbind(diag_out, m1$diag, m2$diag)
}

write.csv(scenario_stats, file.path(output_dir, "F3_sensitivity_scenarios.csv"), row.names = FALSE)
write.csv(ambiguous_cases, file.path(output_dir, "F3_boundary_ambiguous_cases.csv"), row.names = FALSE)
write.csv(diag_out, file.path(output_dir, "F3_logistic_model_diagnostics.csv"), row.names = FALSE)
write.csv(coef_out, file.path(output_dir, "F3_logistic_coefficients.csv"), row.names = FALSE)

cat("F3 complete. Outputs:\n")
cat(" - ", file.path(output_dir, "F3_sensitivity_scenarios.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F3_boundary_ambiguous_cases.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F3_logistic_model_diagnostics.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F3_logistic_coefficients.csv"), "\n", sep = "")
