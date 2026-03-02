################################################################################
# F1 Analysis: China rank correlations and institutional type
# Produces manuscript-facing outputs for Supplemental Materials F1
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

to_int <- function(x) as.integer(as.character(x))

china$check_framework <- to_int(china[["check_framework(0/1)"]])
china$tool_specified <- to_int(china[["tool_specified(0/1)"]])
china$linked_to_pass <- to_int(china[["linked_to_pass(0/1)"]])
china$rank_numeric <- suppressWarnings(as.numeric(china[["shanghairank_2025"]]))

n_total <- nrow(china)
n_adopters <- sum(china$check_framework == 1, na.rm = TRUE)
n_tool <- sum(china$tool_specified == 1, na.rm = TRUE)
n_linked <- sum(china$linked_to_pass == 1, na.rm = TRUE)
n_full_stack <- sum(china$check_framework == 1 & china$tool_specified == 1 & china$linked_to_pass == 1, na.rm = TRUE)

summary_out <- data.frame(
  metric = c(
    "n_total",
    "n_adopters_check_framework",
    "pct_adopters_check_framework",
    "n_tool_specified",
    "pct_tool_specified_among_adopters",
    "n_linked_to_pass",
    "pct_linked_to_pass_among_adopters",
    "n_full_stack"
  ),
  value = c(
    n_total,
    n_adopters,
    100 * n_adopters / n_total,
    n_tool,
    ifelse(n_adopters > 0, 100 * n_tool / n_adopters, NA_real_),
    n_linked,
    ifelse(n_adopters > 0, 100 * n_linked / n_adopters, NA_real_),
    n_full_stack
  )
)

# Top-100 numeric-rank subset used in the supplement
china_top100 <- subset(china, !is.na(rank_numeric) & rank_numeric <= 100)

pointbis <- cor.test(china_top100$check_framework, china_top100$rank_numeric, method = "pearson")
mean_rank_cf0 <- mean(china_top100$rank_numeric[china_top100$check_framework == 0], na.rm = TRUE)
sd_rank_cf0 <- sd(china_top100$rank_numeric[china_top100$check_framework == 0], na.rm = TRUE)
mean_rank_cf1 <- mean(china_top100$rank_numeric[china_top100$check_framework == 1], na.rm = TRUE)
sd_rank_cf1 <- sd(china_top100$rank_numeric[china_top100$check_framework == 1], na.rm = TRUE)

top20 <- subset(china_top100, rank_numeric <= 20)
rank_21_100 <- subset(china_top100, rank_numeric >= 21 & rank_numeric <= 100)

rank_stats <- data.frame(
  n_top100_numeric = nrow(china_top100),
  pointbiserial_r = unname(pointbis$estimate),
  pointbiserial_p = pointbis$p.value,
  mean_rank_no_framework = mean_rank_cf0,
  sd_rank_no_framework = sd_rank_cf0,
  mean_rank_framework = mean_rank_cf1,
  sd_rank_framework = sd_rank_cf1,
  adoption_rate_top20 = mean(top20$check_framework, na.rm = TRUE),
  adoption_rate_rank21_100 = mean(rank_21_100$check_framework, na.rm = TRUE)
)

# Institutional-type cross-tab
type_tab <- as.data.frame(table(china$Inst_type), stringsAsFactors = FALSE)
names(type_tab) <- c("Inst_type", "n_total")
type_adopters <- as.data.frame(table(china$Inst_type[china$check_framework == 1]), stringsAsFactors = FALSE)
names(type_adopters) <- c("Inst_type", "n_adopters")
by_type <- merge(type_tab, type_adopters, by = "Inst_type", all.x = TRUE)
by_type$n_adopters[is.na(by_type$n_adopters)] <- 0
by_type$adoption_rate <- with(by_type, ifelse(n_total > 0, n_adopters / n_total, NA_real_))
by_type <- by_type[order(by_type$n_adopters, decreasing = TRUE), ]

# Enforcement regime distribution
regime <- ifelse(
  china$check_framework == 0, "No mandatory",
  ifelse(china$tool_specified == 1 & china$linked_to_pass == 1, "Full stack",
  ifelse(china$tool_specified == 1 & china$linked_to_pass == 0, "Mandate + tool specified",
  ifelse(china$tool_specified == 0 & china$linked_to_pass == 1, "Mandate + linked-to-pass", "Mandate only")))
)
regime_tab <- as.data.frame(table(regime), stringsAsFactors = FALSE)
names(regime_tab) <- c("regime", "n")
regime_tab$pct <- 100 * regime_tab$n / n_total

write.csv(summary_out, file.path(output_dir, "F1_china_summary.csv"), row.names = FALSE)
write.csv(rank_stats, file.path(output_dir, "F1_china_rank_stats.csv"), row.names = FALSE)
write.csv(by_type, file.path(output_dir, "F1_china_by_institution_type.csv"), row.names = FALSE)
write.csv(regime_tab, file.path(output_dir, "F1_china_enforcement_regimes.csv"), row.names = FALSE)

cat("F1 complete. Outputs:\n")
cat(" - ", file.path(output_dir, "F1_china_summary.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F1_china_rank_stats.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F1_china_by_institution_type.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F1_china_enforcement_regimes.csv"), "\n", sep = "")
