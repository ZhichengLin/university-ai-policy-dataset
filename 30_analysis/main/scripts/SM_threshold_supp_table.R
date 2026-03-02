################################################################################
# Supplemental table formatter: AIGC numeric threshold rules (China)
# Inputs:
#   - SM_china_aigc_threshold_rule_inventory_full.csv
# Outputs:
#   - SM_china_aigc_threshold_numeric_rules_table.csv
#   - SM_china_aigc_threshold_distribution.csv
#   - SM_china_aigc_threshold_numeric_rules_table.md
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
input_file <- normalizePath(
  file.path(root_dir, "30_analysis/main/results/SM_china_aigc_threshold_rule_inventory_full.csv"),
  mustWork = TRUE
)
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_rules_csv <- file.path(output_dir, "SM_china_aigc_threshold_numeric_rules_table.csv")
output_dist_csv <- file.path(output_dir, "SM_china_aigc_threshold_distribution.csv")
output_rules_md <- file.path(output_dir, "SM_china_aigc_threshold_numeric_rules_table.md")

inv <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)
num <- subset(inv, !is.na(threshold_value_pct))

map_applies_to <- function(excerpt) {
  x <- tolower(ifelse(is.na(excerpt), "", excerpt))
  if (grepl("outstanding|excellent|优秀", x, perl = TRUE)) {
    return("Outstanding thesis evaluation")
  }
  if (grepl("humanities|social sciences|人文|社科|文科", x, perl = TRUE)) {
    return("Humanities & social sciences (HSS)")
  }
  if (grepl("science|engineering|medical|理工|工科|医学|医科", x, perl = TRUE)) {
    return("Science/engineering/medical")
  }
  "Unspecified / general"
}

format_pct <- function(pct) {
  if (is.na(pct)) {
    return(NA_real_)
  }
  if (abs(pct - round(pct)) < 1e-9) {
    return(as.integer(round(pct)))
  }
  pct
}

map_decision <- function(decision_point, applies_to) {
  if (applies_to == "Outstanding thesis evaluation") {
    return("Outstanding thesis evaluation")
  }
  if (decision_point == "Unspecified") {
    return("Unspecified in excerpt")
  }
  decision_point
}

tbl <- data.frame(
  ID = num$id,
  University = paste0(num$univ_name_en, "\n", num$univ_name_cn),
  "Applies to" = vapply(num$threshold_text_excerpt, map_applies_to, character(1)),
  "AI threshold (%)" = vapply(num$threshold_value_pct, format_pct, numeric(1)),
  "Decision point" = NA_character_,
  Tool = ifelse(
    num$tool_specified == 1,
    ifelse(nzchar(trimws(num$tool_name)), paste0("Yes (", num$tool_name, ")"), "Yes"),
    "No"
  ),
  "Linked to pass" = ifelse(num$linked_to_pass == 1, "Yes", "No"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
tbl[["Decision point"]] <- mapply(map_decision, num$decision_point, tbl[["Applies to"]], USE.NAMES = FALSE)

# Manual disambiguation for rows where one excerpt spans multiple policy clauses.
row_key <- paste(tbl$ID, num$threshold_value_pct, sep = "_")
tbl[["Applies to"]][row_key == "16_15"] <- "Science/engineering/medical"
tbl[["Applies to"]][row_key == "16_20"] <- "Humanities & social sciences (HSS)"
tbl[["Applies to"]][row_key == "96_15"] <- "Outstanding thesis evaluation"
tbl[["Applies to"]][row_key == "96_20"] <- "General requirement"
tbl[["Decision point"]] <- mapply(map_decision, num$decision_point, tbl[["Applies to"]], USE.NAMES = FALSE)

tbl <- tbl[order(num$threshold_value_pct, tbl$ID), ]
row.names(tbl) <- NULL

dist <- aggregate(
  id ~ threshold_value_pct,
  data = num,
  FUN = function(x) c(rule_count = length(x), universities = length(unique(x)))
)
dist <- data.frame(
  "AI threshold (%)" = dist$threshold_value_pct,
  rule_count = as.integer(dist$id[, "rule_count"]),
  universities = as.integer(dist$id[, "universities"]),
  check.names = FALSE
)
dist <- dist[order(dist[["AI threshold (%)"]]), ]
row.names(dist) <- NULL

write.csv(tbl, output_rules_csv, row.names = FALSE, na = "")
write.csv(dist, output_dist_csv, row.names = FALSE, na = "")

# Simple markdown export for direct manuscript/supplement drafting.
to_md_table <- function(df) {
  cols <- names(df)
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) {
    vals <- trimws(as.character(r))
    vals <- gsub("\n", "<br>", vals, fixed = TRUE)
    vals <- gsub("\\|", "\\\\|", vals)
    paste0("| ", paste(vals, collapse = " | "), " |")
  })
  paste(c(header, sep, rows), collapse = "\n")
}

md_lines <- c(
  "# Cleaned AI Numeric Threshold Table (China)",
  "",
  paste0("Rules: ", nrow(tbl), "; Universities: ", length(unique(tbl$ID))),
  "",
  "## Summary Distribution",
  "",
  to_md_table(dist),
  "",
  "## Rule-Level Inventory (Numeric AI Thresholds Only)",
  "",
  to_md_table(tbl)
)
writeLines(md_lines, output_rules_md, useBytes = TRUE)

cat("Supplemental threshold table formatter complete.\n")
cat("Outputs:\n")
cat(" - ", output_rules_csv, "\n", sep = "")
cat(" - ", output_dist_csv, "\n", sep = "")
cat(" - ", output_rules_md, "\n", sep = "")
cat("Numeric rules:", nrow(tbl), "\n")
cat("Universities:", length(unique(tbl$ID)), "\n")
