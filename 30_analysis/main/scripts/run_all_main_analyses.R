################################################################################
# Run all main analysis scripts
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
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

scripts <- c(
  file.path(script_dir, "F1_analysis.R"),
  file.path(script_dir, "F1_effect_sizes.R"),
  file.path(script_dir, "SM_threshold_inventory.R"),
  file.path(script_dir, "SM_threshold_supp_table.R"),
  file.path(script_dir, "SM_safeguards_coding.R"),
  file.path(script_dir, "SM_safeguards_supp_table.R"),
  file.path(script_dir, "SM_disclosure_unreliability_addon_table.R"),
  file.path(script_dir, "SM_china_policy_subtypes.R"),
  file.path(script_dir, "SM_china_subtypes_by_insttype_table.R"),
  file.path(script_dir, "SM_international_policy_signals_coding.R"),
  file.path(script_dir, "F2_analysis.R"),
  file.path(script_dir, "F3_sensitivity.R"),
  file.path(script_dir, "Fig1v4.R")
)

run_log <- data.frame(
  script = basename(scripts),
  status = NA_character_,
  exit_code = NA_integer_,
  stringsAsFactors = FALSE
)

for (i in seq_along(scripts)) {
  s <- normalizePath(scripts[i], mustWork = TRUE)
  cat("Running:", s, "\n")
  old_wd <- getwd()
  setwd(dirname(s))
  cmd_out <- system2("Rscript", args = basename(s), stdout = TRUE, stderr = TRUE)
  setwd(old_wd)
  exit_code <- attr(cmd_out, "status")
  if (is.null(exit_code)) {
    exit_code <- 0L
  }
  run_log$exit_code[i] <- exit_code
  run_log$status[i] <- ifelse(exit_code == 0, "PASS", "FAIL")
  if (exit_code != 0) {
    write.csv(run_log, file.path(output_dir, "run_all_main_analyses_status.csv"), row.names = FALSE)
    stop("Script failed: ", s, " (exit code ", exit_code, ")")
  }
}

write.csv(run_log, file.path(output_dir, "run_all_main_analyses_status.csv"), row.names = FALSE)
cat("All main analyses completed successfully.\n")
