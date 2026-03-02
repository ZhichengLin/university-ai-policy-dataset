################################################################################
# Supplemental table formatter: China policy subtypes by institutional type
# Inputs:
#   - SM_china_policy_subtypes.csv
# Outputs:
#   - SM_china_policy_subtypes_by_insttype_table.csv
#   - SM_china_policy_subtypes_by_insttype_table.md
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
  file.path(root_dir, "30_analysis/main/results/SM_china_policy_subtypes.csv"),
  mustWork = TRUE
)
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_csv <- file.path(output_dir, "SM_china_policy_subtypes_by_insttype_table.csv")
output_md <- file.path(output_dir, "SM_china_policy_subtypes_by_insttype_table.md")

d <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)

required_cols <- c("Inst_type", "policy_subtype")
missing_cols <- setdiff(required_cols, names(d))
if (length(missing_cols) > 0) {
  stop("Missing required columns in subtype input: ", paste(missing_cols, collapse = ", "))
}

subtype_levels <- c(
  "Mandated detection",
  "Detection mentioned, not mandated",
  "AI governance present, no detection",
  "No AI policy found in corpus"
)

fmt_np <- function(n, N) {
  pct <- sprintf("%.1f%%", ifelse(N == 0, 0, 100 * n / N))
  paste0(n, "\n(", pct, ")")
}

format_inst_label <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s+universit(y|ies)$", "", x, ignore.case = TRUE)
  tools::toTitleCase(x)
}

inst_types <- sort(unique(d$Inst_type))

rows <- lapply(inst_types, function(inst) {
  di <- d[d$Inst_type == inst, , drop = FALSE]
  N <- nrow(di)
  counts <- sapply(subtype_levels, function(s) sum(di$policy_subtype == s, na.rm = TRUE))
  c(
    "Institutional type" = format_inst_label(inst),
    "n" = as.character(N),
    "Mandated detection" = fmt_np(counts["Mandated detection"], N),
    "Detection mentioned, not mandated" = fmt_np(counts["Detection mentioned, not mandated"], N),
    "AI governance present, no detection" = fmt_np(counts["AI governance present, no detection"], N),
    "No AI policy found" = fmt_np(counts["No AI policy found in corpus"], N)
  )
})

N_total <- nrow(d)
counts_total <- sapply(subtype_levels, function(s) sum(d$policy_subtype == s, na.rm = TRUE))
row_total <- c(
  "Institutional type" = "Total",
  "n" = as.character(N_total),
  "Mandated detection" = fmt_np(counts_total["Mandated detection"], N_total),
  "Detection mentioned, not mandated" = fmt_np(counts_total["Detection mentioned, not mandated"], N_total),
  "AI governance present, no detection" = fmt_np(counts_total["AI governance present, no detection"], N_total),
  "No AI policy found" = fmt_np(counts_total["No AI policy found in corpus"], N_total)
)

table_out <- as.data.frame(do.call(rbind, c(rows, list(row_total))), stringsAsFactors = FALSE)
row.names(table_out) <- NULL

write.csv(table_out, output_csv, row.names = FALSE, na = "")

to_md_table <- function(df) {
  cols <- names(df)
  cols[cols == "n"] <- "*n*"
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|")
  rows_md <- apply(df, 1, function(r) {
    vals <- as.character(r)
    vals <- gsub("\n", "<br>", vals, fixed = TRUE)
    vals <- gsub("\\|", "\\\\|", vals)
    paste0("| ", paste(vals, collapse = " | "), " |")
  })
  paste(c(header, sep, rows_md), collapse = "\n")
}

md_lines <- c(
  "# Table",
  "",
  "Table. Institutional-type distribution of AI policy subtypes in Chinese universities (*n* = 128).",
  "",
  "By institutional type (counts with within-type percentages)",
  "",
  to_md_table(table_out)
)
writeLines(md_lines, output_md, useBytes = TRUE)

cat("Institutional-type subtype table complete.\n")
cat("Outputs:\n")
cat(" - ", output_csv, "\n", sep = "")
cat(" - ", output_md, "\n", sep = "")
cat("Rows:", nrow(table_out), "\n")
