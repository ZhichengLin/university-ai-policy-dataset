################################################################################
# F2 Analysis: International country-level variation and maturity score
# Produces manuscript-facing outputs for Supplemental Materials F2
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
input_file <- normalizePath(file.path(root_dir, "20_coding/21_store/S2_international_analysis.csv"), mustWork = TRUE)
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

intl <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

intl$D1 <- to_num(intl[["D1_tech_limit(0/1)"]])
intl$D2 <- to_num(intl[["D2_proactive(0/1)"]])
intl$D3 <- to_num(intl[["D3_responsibility(0/1)"]])
intl$D4 <- to_num(intl[["D4_ai_as_aid(0/1)"]])
intl$maturity <- to_num(intl[["maturity_score"]])
intl$rank <- to_num(intl[["rank_usnews_2025"]])

n_total <- nrow(intl)

dim_prev <- data.frame(
  dimension = c("D1_tech_limit", "D2_proactive", "D3_responsibility", "D4_ai_as_aid"),
  n_positive = c(sum(intl$D1, na.rm = TRUE), sum(intl$D2, na.rm = TRUE), sum(intl$D3, na.rm = TRUE), sum(intl$D4, na.rm = TRUE))
)
dim_prev$pct_positive <- 100 * dim_prev$n_positive / n_total

country_n <- as.data.frame(table(intl$country_region), stringsAsFactors = FALSE)
names(country_n) <- c("country_region", "n")

country_sum <- aggregate(
  cbind(D1, D2, D3, D4) ~ country_region,
  data = intl,
  FUN = function(x) sum(x, na.rm = TRUE)
)

country_matrix <- merge(country_n, country_sum, by = "country_region", all.x = TRUE)
country_matrix <- country_matrix[order(country_matrix$n, decreasing = TRUE), ]
country_matrix$D1_pct <- 100 * country_matrix$D1 / country_matrix$n
country_matrix$D2_pct <- 100 * country_matrix$D2 / country_matrix$n
country_matrix$D3_pct <- 100 * country_matrix$D3 / country_matrix$n
country_matrix$D4_pct <- 100 * country_matrix$D4 / country_matrix$n

maturity_dist <- as.data.frame(table(intl$maturity), stringsAsFactors = FALSE)
names(maturity_dist) <- c("maturity_score", "n")
maturity_dist$pct <- 100 * maturity_dist$n / n_total

reg_data <- subset(intl, !is.na(rank) & !is.na(maturity))
reg_fit <- lm(maturity ~ rank, data = reg_data)
reg_coef <- summary(reg_fit)$coefficients
regression_out <- data.frame(
  n = nrow(reg_data),
  slope_rank = unname(reg_coef["rank", "Estimate"]),
  slope_se = unname(reg_coef["rank", "Std. Error"]),
  slope_t = unname(reg_coef["rank", "t value"]),
  slope_p = unname(reg_coef["rank", "Pr(>|t|)"]),
  intercept = unname(reg_coef["(Intercept)", "Estimate"]),
  r_squared = summary(reg_fit)$r.squared,
  adj_r_squared = summary(reg_fit)$adj.r.squared
)

# Countries explicitly discussed in manuscript/supplement
focus_countries <- c("USA", "UK", "Singapore", "Japan", "Canada", "Sweden", "Norway", "Denmark", "France", "Australia")
focus_matrix <- subset(country_matrix, country_region %in% focus_countries)

write.csv(dim_prev, file.path(output_dir, "F2_international_dimension_prevalence.csv"), row.names = FALSE)
write.csv(country_matrix, file.path(output_dir, "F2_international_country_dimension_matrix.csv"), row.names = FALSE)
write.csv(focus_matrix, file.path(output_dir, "F2_international_country_focus.csv"), row.names = FALSE)
write.csv(maturity_dist, file.path(output_dir, "F2_international_maturity_distribution.csv"), row.names = FALSE)
write.csv(regression_out, file.path(output_dir, "F2_international_maturity_regression.csv"), row.names = FALSE)

cat("F2 complete. Outputs:\n")
cat(" - ", file.path(output_dir, "F2_international_dimension_prevalence.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F2_international_country_dimension_matrix.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F2_international_country_focus.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F2_international_maturity_distribution.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F2_international_maturity_regression.csv"), "\n", sep = "")
