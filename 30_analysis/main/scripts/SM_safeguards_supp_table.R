################################################################################
# Supplemental table formatter: Procedural safeguards by enforcement regime
# Inputs:
#   - SM_china_safeguards_coding.csv
# Outputs:
#   - SM_china_safeguards_table_S3.csv
#   - SM_china_safeguards_table_S3.md
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
  file.path(root_dir, "30_analysis/main/results/SM_china_safeguards_coding.csv"),
  mustWork = TRUE
)
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_csv <- file.path(output_dir, "SM_china_safeguards_table_S3.csv")
output_md <- file.path(output_dir, "SM_china_safeguards_table_S3.md")

d <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)

# S3 is defined for mandated-detection adopters only (check_framework == 1).
if ("sample_group" %in% names(d)) {
  d <- d[d$sample_group == "mandated_detection", , drop = FALSE]
} else if ("check_framework" %in% names(d)) {
  d <- d[d$check_framework == 1, , drop = FALSE]
}

required_cols <- c(
  "tool_specified",
  "linked_to_pass",
  "human_review_requirement",
  "appeal_right_reply",
  "transparency_records",
  "detector_not_sole_evidence",
  "error_limitation_disclosure",
  "safeguard_coverage_score"
)
missing_cols <- setdiff(required_cols, names(d))
if (length(missing_cols) > 0) {
  stop("Missing required columns in safeguards input: ", paste(missing_cols, collapse = ", "))
}

d$regime <- ifelse(
  d$tool_specified == 0 & d$linked_to_pass == 0, "Mandate only (no tool, no linkage)",
  ifelse(
    d$tool_specified == 0 & d$linked_to_pass == 1, "Mandate + linkage (no tool specified)",
    ifelse(
      d$tool_specified == 1 & d$linked_to_pass == 0, "Mandate + tool (no linkage)",
      ifelse(
        d$tool_specified == 1 & d$linked_to_pass == 1, "Full stack (tool + linkage)",
        "Unclassified"
      )
    )
  )
)

regime_levels <- c(
  "Mandate only (no tool, no linkage)",
  "Mandate + linkage (no tool specified)",
  "Mandate + tool (no linkage)",
  "Full stack (tool + linkage)"
)

regime_n <- sapply(regime_levels, function(r) sum(d$regime == r, na.rm = TRUE))
total_n <- nrow(d)

fmt_np <- function(n, N) {
  pct <- sprintf("%.1f%%", ifelse(N == 0, 0, 100 * n / N))
  paste0(n, "\n(", pct, ")")
}

fmt_num <- function(x) {
  if (is.na(x)) {
    return("")
  }
  if (abs(x - round(x)) < 1e-9) {
    return(as.character(as.integer(round(x))))
  }
  txt <- format(round(x, 2), nsmall = 0, trim = TRUE, scientific = FALSE)
  sub("\\.?0+$", "", txt)
}

fmt_median_iqr <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) {
    return("NA")
  }
  med <- median(v)
  q <- quantile(v, probs = c(0.25, 0.75), names = FALSE, type = 7)
  paste0(fmt_num(med), " (", fmt_num(q[1]), "-", fmt_num(q[2]), ")")
}

safeguard_vars <- c(
  "human_review_requirement",
  "appeal_right_reply",
  "transparency_records",
  "detector_not_sole_evidence",
  "error_limitation_disclosure"
)

safeguard_labels <- c(
  "Mandatory human review of flagged work (Box 1.2)",
  "Appeal / right of reply (Box 1.2 / 1.7)",
  "Tool / threshold / report transparency recorded in student file (Box 1.3)",
  "Explicit \"not sole evidence\" / reference-only / non-dispositive statement (Box 1.1)",
  "Error/limitation disclosure or link to limitations note (Box 1.4)"
)

col_all <- paste0("All adopters (n=", total_n, ")")
col_regime <- paste0(regime_levels, " (n=", as.integer(regime_n), ")")

make_safeguard_row <- function(var_name, label) {
  row <- c(label, fmt_np(sum(d[[var_name]] == 1, na.rm = TRUE), total_n))
  for (r in regime_levels) {
    dr <- d[d$regime == r, , drop = FALSE]
    row <- c(row, fmt_np(sum(dr[[var_name]] == 1, na.rm = TRUE), nrow(dr)))
  }
  row
}

rows <- Map(make_safeguard_row, safeguard_vars, safeguard_labels)

score_row <- c(
  "Safeguard coverage score, median (IQR)dagger",
  fmt_median_iqr(d$safeguard_coverage_score)
)
for (r in regime_levels) {
  dr <- d[d$regime == r, , drop = FALSE]
  score_row <- c(score_row, fmt_median_iqr(dr$safeguard_coverage_score))
}

ge1_row <- c(
  "Policies with >=1 safeguard (score >=1)",
  fmt_np(sum(d$safeguard_coverage_score >= 1, na.rm = TRUE), total_n)
)
for (r in regime_levels) {
  dr <- d[d$regime == r, , drop = FALSE]
  ge1_row <- c(ge1_row, fmt_np(sum(dr$safeguard_coverage_score >= 1, na.rm = TRUE), nrow(dr)))
}

ge2_row <- c(
  "Policies with >=2 safeguards (score >=2)",
  fmt_np(sum(d$safeguard_coverage_score >= 2, na.rm = TRUE), total_n)
)
for (r in regime_levels) {
  dr <- d[d$regime == r, , drop = FALSE]
  ge2_row <- c(ge2_row, fmt_np(sum(dr$safeguard_coverage_score >= 2, na.rm = TRUE), nrow(dr)))
}

table_s3 <- as.data.frame(
  rbind(do.call(rbind, rows), score_row, ge1_row, ge2_row),
  stringsAsFactors = FALSE
)
names(table_s3) <- c("Safeguard aligned to Box 1", col_all, col_regime)

write.csv(table_s3, output_csv, row.names = FALSE, na = "")

to_md_table <- function(df) {
  cols <- names(df)
  cols <- gsub("\\(n=", "(*n*=", cols)
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) {
    vals <- as.character(r)
    vals <- gsub("\n", "<br>", vals, fixed = TRUE)
    vals <- gsub("\\|", "\\\\|", vals)
    paste0("| ", paste(vals, collapse = " | "), " |")
  })
  paste(c(header, sep, rows), collapse = "\n")
}

caption <- paste0(
  "Table. Procedural safeguards explicitly stated in Chinese adopter policies, ",
  "by enforcement regime (n = ", total_n, ")"
)

md_lines <- c(
  "# Table",
  "",
  caption,
  "",
  "Caption: Procedural safeguards present in Chinese university policies that mandate AI-content detection for graduation theses (*n* = 29).",
  "We coded whether policies explicitly contain five procedural safeguards aligned to Box 1 (main text).",
  "Cells report n (% within regime). Enforcement regimes follow Fig. 1A.",
  "",
  to_md_table(table_s3),
  "",
  "dagger Coverage score = sum of the five binary safeguards (0-5).",
  "",
  "Footnote: Coding is based on whether safeguards are explicitly stated in publicly accessible policy text (0/1);",
  "0 indicates absence of explicit language, not absence in practice.",
  "",
  "Footnote: \"Transparency recorded\" captures whether the policy requires archiving the detector report and/or",
  "recording the tool/threshold/output in the student record; it does not imply that tool versions or error",
  "profiles are systematically documented."
)
writeLines(md_lines, output_md, useBytes = TRUE)

cat("Supplemental safeguards table formatter complete.\n")
cat("Outputs:\n")
cat(" - ", output_csv, "\n", sep = "")
cat(" - ", output_md, "\n", sep = "")
cat("Rows:", nrow(table_s3), "\n")
cat("N:", total_n, "\n")
