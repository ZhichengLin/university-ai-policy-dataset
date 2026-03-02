################################################################################
# Supplemental add-on table: AI-use disclosure statement and detector
# unreliability warning across mandated-detection + governance-only policies.
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
china_input_file <- normalizePath(
  file.path(root_dir, "30_analysis/main/results/SM_china_safeguards_coding.csv"),
  mustWork = TRUE
)
intl_input_file <- normalizePath(
  file.path(root_dir, "20_coding/21_store/S2_international_analysis.csv"),
  mustWork = TRUE
)
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_csv <- file.path(output_dir, "SM_china_disclosure_unreliability_addon_table.csv")
output_md <- file.path(output_dir, "SM_china_disclosure_unreliability_addon_table.md")

d <- read.csv(china_input_file, stringsAsFactors = FALSE, check.names = FALSE)
intl <- read.csv(intl_input_file, stringsAsFactors = FALSE, check.names = FALSE)

required_cols <- c(
  "sample_group",
  "ai_use_disclosure_statement",
  "error_limitation_disclosure",
  "d3_responsibility",
  "d4_ai_as_aid"
)
missing_cols <- setdiff(required_cols, names(d))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

d <- d[d$sample_group %in% c("mandated_detection", "non_detection_governance"), , drop = FALSE]

all_n <- nrow(d)
mandated <- d[d$sample_group == "mandated_detection", , drop = FALSE]
governance <- d[d$sample_group == "non_detection_governance", , drop = FALSE]
mandated_n <- nrow(mandated)
governance_n <- nrow(governance)
intl_n <- nrow(intl)

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
intl$D1 <- to_num(intl[["D1_tech_limit(0/1)"]])
intl$D2 <- to_num(intl[["D2_proactive(0/1)"]])
intl$D3 <- to_num(intl[["D3_responsibility(0/1)"]])
intl$D4 <- to_num(intl[["D4_ai_as_aid(0/1)"]])

fmt_np <- function(n, N) {
  pct <- ifelse(N == 0, 0, 100 * n / N)
  paste0(n, "\n(", sprintf("%.1f%%", pct), ")")
}
fmt_mean <- function(x) {
  if (is.na(x)) {
    return("")
  }
  sprintf("%.2f", x)
}

intl_score <- rowSums(intl[, c("D1", "D2", "D3", "D4")], na.rm = TRUE)
china_score <- d$error_limitation_disclosure + d$ai_use_disclosure_statement + d$d3_responsibility + d$d4_ai_as_aid
mandated_score <- mandated$error_limitation_disclosure + mandated$ai_use_disclosure_statement + mandated$d3_responsibility + mandated$d4_ai_as_aid
governance_score <- governance$error_limitation_disclosure + governance$ai_use_disclosure_statement + governance$d3_responsibility + governance$d4_ai_as_aid

rows <- list(
  c(
    "D1. Detector unreliability/error-limitation warning stated",
    fmt_np(sum(intl$D1 == 1, na.rm = TRUE), intl_n),
    fmt_np(sum(d$error_limitation_disclosure == 1, na.rm = TRUE), all_n),
    fmt_np(sum(mandated$error_limitation_disclosure == 1, na.rm = TRUE), mandated_n),
    fmt_np(sum(governance$error_limitation_disclosure == 1, na.rm = TRUE), governance_n)
  ),
  c(
    "D2. AI-use disclosure/declaration statement required",
    fmt_np(sum(intl$D2 == 1, na.rm = TRUE), intl_n),
    fmt_np(sum(d$ai_use_disclosure_statement == 1, na.rm = TRUE), all_n),
    fmt_np(sum(mandated$ai_use_disclosure_statement == 1, na.rm = TRUE), mandated_n),
    fmt_np(sum(governance$ai_use_disclosure_statement == 1, na.rm = TRUE), governance_n)
  ),
  c(
    "D3. Student retains responsibility for submitted content",
    fmt_np(sum(intl$D3 == 1, na.rm = TRUE), intl_n),
    fmt_np(sum(d$d3_responsibility == 1, na.rm = TRUE), all_n),
    fmt_np(sum(mandated$d3_responsibility == 1, na.rm = TRUE), mandated_n),
    fmt_np(sum(governance$d3_responsibility == 1, na.rm = TRUE), governance_n)
  ),
  c(
    "D4. AI positioned as permissible aid under conditions",
    fmt_np(sum(intl$D4 == 1, na.rm = TRUE), intl_n),
    fmt_np(sum(d$d4_ai_as_aid == 1, na.rm = TRUE), all_n),
    fmt_np(sum(mandated$d4_ai_as_aid == 1, na.rm = TRUE), mandated_n),
    fmt_np(sum(governance$d4_ai_as_aid == 1, na.rm = TRUE), governance_n)
  ),
  c(
    "Mean D1-D4 score (0-4)",
    fmt_mean(mean(intl_score, na.rm = TRUE)),
    fmt_mean(mean(china_score, na.rm = TRUE)),
    fmt_mean(mean(mandated_score, na.rm = TRUE)),
    fmt_mean(mean(governance_score, na.rm = TRUE))
  )
)

tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
names(tab) <- c(
  "Dimension",
  paste0("International (N = ", intl_n, ")"),
  paste0("China combined (mandated + non-detection governance) (N = ", all_n, ")"),
  paste0("China mandated detection (N = ", mandated_n, ")"),
  paste0("China non-detection governance (N = ", governance_n, ")")
)

write.csv(tab, output_csv, row.names = FALSE, na = "")

to_md_table <- function(df) {
  md_names <- gsub("(N = ", "(*N* = ", names(df), fixed = TRUE)
  header <- paste0("| ", paste(md_names, collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) {
    vals <- as.character(r)
    vals <- gsub("\n", "<br>", vals, fixed = TRUE)
    vals <- gsub("\\|", "\\\\|", vals)
    paste0("| ", paste(vals, collapse = " | "), " |")
  })
  paste(c(header, sep, rows), collapse = "\n")
}

md_lines <- c(
  "# Table",
  "",
  "Table. D1-D4 contrast across international and China policy subsets.",
  "",
  to_md_table(tab),
  "",
  "Note. Cells report n (% within group). D1 captures explicit language about detector technical limits or uncertain accuracy. D2 captures explicit requirements to document AI-tool use in thesis statements/forms/sections. D3 captures explicit statements that students retain responsibility for submitted work. D4 captures explicit framing of AI as a permissible aid under conditions."
)
writeLines(md_lines, output_md, useBytes = TRUE)

cat("Add-on table complete.\n")
cat("Outputs:\n")
cat(" - ", output_csv, "\n", sep = "")
cat(" - ", output_md, "\n", sep = "")
cat("N all:", all_n, "\n")
cat("N mandated:", mandated_n, "\n")
cat("N governance:", governance_n, "\n")
cat("N international:", intl_n, "\n")
