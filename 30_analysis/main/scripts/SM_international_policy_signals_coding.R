################################################################################
# Supplemental coding: International policy enforcement/disclosure signals
# Indicators coded (strict text-anchored, from policy markdown):
# 1) Required disclosure template OR standardized statement language
# 2) Faculty procedures for suspected AI misuse
# 3) Sanctions for nondisclosure (including attribution-failure language)
# 4) Detector mention at all
# 5) "Not sole evidence" language for detector outputs
# 6) Required detector screening anywhere
#
# Inputs:
#   - 20_coding/21_store/S2_international_analysis.csv
#   - 00_source_material/04_policy_markdown/02_international_original/*.md
#
# Outputs:
#   - 30_analysis/main/results/SM_international_policy_signals_coding.csv
#   - 30_analysis/main/results/SM_international_policy_signals_evidence.csv
#   - 30_analysis/main/results/SM_international_policy_signals_summary.csv
#   - 30_analysis/main/results/SM_international_policy_signals_summary.md
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

sanitize_text <- function(x) {
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

split_clauses <- function(text) {
  pieces <- unlist(strsplit(text, "(?<=[\\.!?;:。；])\\s+|\\n+", perl = TRUE))
  pieces <- trimws(pieces)
  pieces[nzchar(pieces)]
}

first_single_hit <- function(text, pattern) {
  if (!nzchar(text)) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  clauses <- split_clauses(text)
  for (cl in clauses) {
    m <- regexpr(pattern, cl, perl = TRUE, ignore.case = TRUE)
    if (m[1] != -1) {
      kw <- regmatches(cl, m)[1]
      return(list(hit = 1L, keyword = sanitize_text(kw), excerpt = sanitize_text(cl)))
    }
  }
  m <- regexpr(pattern, text, perl = TRUE, ignore.case = TRUE)
  if (m[1] == -1) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  start <- max(1L, m[1] - 140L)
  end <- min(nchar(text), m[1] + attr(m, "match.length") + 180L)
  ex <- substr(text, start, end)
  kw <- regmatches(text, m)[1]
  list(hit = 1L, keyword = sanitize_text(kw), excerpt = sanitize_text(ex))
}

first_multi_hit <- function(text, patterns, window = 3L) {
  if (!nzchar(text)) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  clauses <- split_clauses(text)
  n <- length(clauses)
  if (n == 0) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  for (i in seq_len(n)) {
    jmax <- min(n, i + window - 1L)
    for (j in i:jmax) {
      chunk <- paste(clauses[i:j], collapse = " ")
      ok <- all(vapply(
        patterns,
        function(p) grepl(p, chunk, perl = TRUE, ignore.case = TRUE),
        logical(1)
      ))
      if (!ok) {
        next
      }
      kw <- ""
      for (p in patterns) {
        m <- regexpr(p, chunk, perl = TRUE, ignore.case = TRUE)
        if (m[1] != -1) {
          kw <- sanitize_text(regmatches(chunk, m)[1])
          break
        }
      }
      return(list(hit = 1L, keyword = kw, excerpt = sanitize_text(chunk)))
    }
  }
  list(hit = 0L, keyword = "", excerpt = "")
}

to_md_table <- function(df) {
  cols <- names(df)
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) {
    vals <- as.character(r)
    vals <- gsub("\\|", "\\\\|", vals)
    paste0("| ", paste(vals, collapse = " | "), " |")
  })
  paste(c(header, sep, rows), collapse = "\n")
}

find_markdown_relpath <- function(snapshot_filename, rank_value, md_dir_rel, md_dir_abs) {
  if (!is.na(snapshot_filename) && nzchar(trimws(snapshot_filename))) {
    md_name <- sub("\\.(html|pdf)$", ".md", snapshot_filename, ignore.case = TRUE)
    cand_rel <- file.path(md_dir_rel, md_name)
    cand_abs <- file.path(md_dir_abs, md_name)
    if (file.exists(cand_abs)) {
      return(cand_rel)
    }
  }
  rank_num <- suppressWarnings(as.integer(rank_value))
  if (!is.na(rank_num)) {
    hits <- list.files(md_dir_abs, pattern = paste0("^", rank_num, "_.*\\.md$"), full.names = FALSE)
    if (length(hits) == 1) {
      return(file.path(md_dir_rel, hits[1]))
    }
  }
  NA_character_
}

script_dir <- resolve_script_dir()
root_dir <- normalizePath(file.path(script_dir, "../../.."), mustWork = TRUE)
input_file <- normalizePath(file.path(root_dir, "20_coding/21_store/S2_international_analysis.csv"), mustWork = TRUE)
md_dir_rel <- "00_source_material/04_policy_markdown/02_international_original"
md_dir_abs <- normalizePath(file.path(root_dir, md_dir_rel), mustWork = TRUE)
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_coding <- file.path(output_dir, "SM_international_policy_signals_coding.csv")
output_evidence <- file.path(output_dir, "SM_international_policy_signals_evidence.csv")
output_summary <- file.path(output_dir, "SM_international_policy_signals_summary.csv")
output_summary_md <- file.path(output_dir, "SM_international_policy_signals_summary.md")
output_table_csv <- file.path(output_dir, "SM_international_policy_signals_table.csv")
output_table_md <- file.path(output_dir, "SM_international_policy_signals_table.md")

intl <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)

# Strict patterns aligned to user specification.
p_disclosure_template <- paste0(
  "(",
  "declaration template|template for declaration|",
  "disclosure statement|usage statement|",
  "statement of acknowledg(e)?ment|statement to acknowledge|",
  "honou?r declaration(\\s+form)?|",
  "example statement|sample statement|standardi[sz]ed statement|",
  "check\\s+(the\\s+)?box|",
  "appendix[^\\n\\r]{0,80}(declar|disclos|ai use)|",
  "declar[^\\n\\r]{0,80}template",
  ")"
)
p_actor <- paste0(
  "(",
  "faculty|instructor|teacher|staff|marker|convenor|",
  "subject coordinators?|course coordinators?|integrity officer",
  ")"
)
p_suspicion <- paste0(
  "(",
  "suspect|suspected|suspicion|flagged|allegation|",
  "indications? of misconduct|unauthorized use|",
  "concerns?[^\\n\\r]{0,60}(submission|work|misconduct|unauthorized|ai output|genai)|",
  "concerns? that[^\\n\\r]{0,80}(submission|work)[^\\n\\r]{0,80}(ai|genai)",
  ")"
)
p_procedure <- paste0(
  "(",
  "ask[^\\n\\r]{0,40}(explain|conversation)|",
  "discuss[^\\n\\r]{0,40}student|investigatory viva|",
  "investigat[^\\n\\r]{0,80}(office|integrity officer|conduct|honou?r|council|board)|",
  "refer[^\\n\\r]{0,60}(conduct|honou?r|integrity|examination board)|",
  "gather evidence|follow up|",
  "report[^\\n\\r]{0,60}(conduct|honou?r|integrity|council|office)",
  ")"
)
p_nondisclosure <- paste0(
  "(",
  "not appropriately attributed|without attribution|without disclos(?:ure|ing)|",
  "failure to disclos(?:e|ure)|fail to disclos(?:e|ure)|",
  "hide[^\\n\\r]{0,30}(ai|use)|not transparent",
  ")"
)
p_sanction <- paste0(
  "(",
  "academic misconduct|misconduct|penalt(y|ies)|disciplin|hearing|",
  "suspension|expulsion|grade reduction|grade penalty|revo(k|c)e|possible penalties",
  ")"
)
p_detector_mention <- paste0(
  "(",
  "ai\\s*(content\\s*)?detectors?|ai\\s*detection(\\s*(tool|tools|software|score|indicator))?|",
  "ai\\s*writing\\s*indicator|ai\\s*writing\\s*detection|",
  "genai\\s*detectors?|generative\\s*ai\\s*detectors?|",
  "turnitin[^\\n\\r]{0,140}(",
    "ai\\s*writing\\s*indicator|",
    "ai\\s*detection|",
    "detect[^\\n\\r]{0,60}(artificial intelligence|generative ai|ai-generated)|",
    "tool[^\\n\\r]{0,80}detect[^\\n\\r]{0,60}(artificial intelligence|generative ai|ai-generated)|",
    "probability[^\\n\\r]{0,100}(generative ai|ai\\s*tool)",
  ")|",
  "detection software[^\\n\\r]{0,120}(review|screen|check|assess|evaluate)[^\\n\\r]{0,120}(generative ai|genai|ai use|ai-generated|ai output)",
  ")"
)
p_not_sole <- paste0(
  "(",
  "never be the sole evidence|not be the only evidence|cannot be based solely|",
  "not a grading metric|insufficient[^\\n\\r]{0,80}(detector|detection)|",
  "considered alongside other relevant evidence|",
  "not proof[^\\n\\r]{0,80}(misconduct|academic misconduct)",
  ")"
)
p_required_screen <- paste0(
  "(",
  "(must|required|mandatory|shall)[^\\.;:]{0,80}",
  "(use|run|apply|submit|screen|check)[^\\.;:]{0,80}",
  "(turnitin|ai\\s*detector|ai\\s*detection|detection software|genai detector)",
  ")"
)

intl$markdown_relpath <- mapply(
  find_markdown_relpath,
  intl$snapshot_filename,
  intl$rank_usnews_2025,
  MoreArgs = list(md_dir_rel = md_dir_rel, md_dir_abs = md_dir_abs),
  USE.NAMES = FALSE
)
intl$markdown_found <- as.integer(!is.na(intl$markdown_relpath))

indicator_keys <- c(
  "required_disclosure_template",
  "faculty_procedure_suspected_ai_misuse",
  "sanctions_for_nondisclosure",
  "detector_mention_any",
  "detector_not_sole_evidence",
  "required_detector_screening"
)

indicator_labels <- c(
  required_disclosure_template = "Required disclosure template OR standardized statement language",
  faculty_procedure_suspected_ai_misuse = "Faculty procedures for suspected AI misuse",
  sanctions_for_nondisclosure = "Sanctions for nondisclosure (incl. attribution failure)",
  detector_mention_any = "Detector mention at all",
  detector_not_sole_evidence = "Detector explicitly not sole evidence",
  required_detector_screening = "Required detector screening anywhere"
)

coding_rows <- vector("list", length = nrow(intl))
evidence_rows <- vector("list", length = nrow(intl) * length(indicator_keys))
e_idx <- 1L

for (i in seq_len(nrow(intl))) {
  relp <- intl$markdown_relpath[i]
  abs_path <- if (!is.na(relp)) file.path(root_dir, relp) else NA_character_
  text <- ""
  if (!is.na(abs_path) && file.exists(abs_path)) {
    lines <- readLines(abs_path, warn = FALSE, encoding = "UTF-8")
    full_idx <- grep("^##\\s+Full Policy Text", lines)
    if (length(full_idx) > 0 && full_idx[1] < length(lines)) {
      text <- paste(lines[(full_idx[1] + 1):length(lines)], collapse = "\n")
    } else {
      text <- paste(lines, collapse = "\n")
    }
    text <- sanitize_text(text)
  }

  h_disclosure <- first_single_hit(text, p_disclosure_template)
  h_faculty <- first_multi_hit(text, c(p_actor, p_suspicion, p_procedure), window = 3L)
  h_sanctions <- first_multi_hit(text, c(p_nondisclosure, p_sanction), window = 1L)
  h_nd <- first_single_hit(text, p_nondisclosure)
  h_sanc <- first_single_hit(text, p_sanction)
  # Broaden to document-level "sanctions hook":
  # if nondisclosure/attribution-failure and misconduct/penalty language both exist
  # in the same policy text, code as present even if split across clauses.
  if (h_sanctions$hit == 0L && h_nd$hit == 1L && h_sanc$hit == 1L) {
    h_sanctions$hit <- 1L
    h_sanctions$keyword <- sanitize_text(paste(h_nd$keyword, "|", h_sanc$keyword))
    h_sanctions$excerpt <- sanitize_text(paste(
      "Nondisclosure evidence:", h_nd$excerpt,
      "Sanction hook evidence:", h_sanc$excerpt
    ))
  }
  h_detector <- first_single_hit(text, p_detector_mention)
  h_not_sole <- first_single_hit(text, p_not_sole)
  h_screen <- first_single_hit(text, p_required_screen)

  row <- data.frame(
    id = intl$id[i],
    univ_name = intl$univ_name[i],
    country_region = intl$country_region[i],
    rank_usnews_2025 = intl$rank_usnews_2025[i],
    source_url = intl$source_url[i],
    markdown_relpath = ifelse(is.na(relp), "", relp),
    text_source_used = ifelse(nzchar(text), "policy_markdown", "missing_markdown"),
    required_disclosure_template = h_disclosure$hit,
    faculty_procedure_suspected_ai_misuse = h_faculty$hit,
    sanctions_for_nondisclosure = h_sanctions$hit,
    detector_mention_any = h_detector$hit,
    detector_not_sole_evidence = h_not_sole$hit,
    required_detector_screening = h_screen$hit,
    policy_signal_score = h_disclosure$hit + h_faculty$hit + h_sanctions$hit + h_detector$hit + h_not_sole$hit + h_screen$hit,
    needs_manual_check = as.integer(!nzchar(text)),
    stringsAsFactors = FALSE
  )
  coding_rows[[i]] <- row

  hit_list <- list(
    required_disclosure_template = h_disclosure,
    faculty_procedure_suspected_ai_misuse = h_faculty,
    sanctions_for_nondisclosure = h_sanctions,
    detector_mention_any = h_detector,
    detector_not_sole_evidence = h_not_sole,
    required_detector_screening = h_screen
  )

  for (k in indicator_keys) {
    evidence_rows[[e_idx]] <- data.frame(
      id = intl$id[i],
      univ_name = intl$univ_name[i],
      indicator = k,
      indicator_label = indicator_labels[[k]],
      code_final = hit_list[[k]]$hit,
      text_source_used = ifelse(nzchar(text), "policy_markdown", "missing_markdown"),
      evidence_keyword = hit_list[[k]]$keyword,
      evidence_excerpt = hit_list[[k]]$excerpt,
      stringsAsFactors = FALSE
    )
    e_idx <- e_idx + 1L
  }
}

coding <- do.call(rbind, coding_rows)
coding <- coding[order(coding$id), ]
row.names(coding) <- NULL

evidence <- do.call(rbind, evidence_rows)
evidence <- evidence[order(evidence$id, evidence$indicator), ]
row.names(evidence) <- NULL

n_total <- nrow(coding)
summary_df <- data.frame(
  indicator = indicator_keys,
  indicator_label = unname(indicator_labels[indicator_keys]),
  n_positive = as.integer(colSums(coding[, indicator_keys], na.rm = TRUE)),
  stringsAsFactors = FALSE
)
summary_df$pct_positive <- round(100 * summary_df$n_positive / n_total, 1)
summary_df$display <- paste0(summary_df$n_positive, " (", sprintf("%.1f%%", summary_df$pct_positive), ")")
summary_df$definition <- ""
summary_df$definition[summary_df$indicator == "required_disclosure_template"] <-
  "Code 1 when policy explicitly requires either (a) a disclosure template/form or (b) standardized statement language."
summary_df$definition[summary_df$indicator == "faculty_procedure_suspected_ai_misuse"] <-
  "Code 1 only when policy explicitly states what faculty/instructors should do when AI misuse is suspected (e.g., conversation, evidence-gathering, referral, investigation/viva)."
summary_df$definition[summary_df$indicator == "sanctions_for_nondisclosure"] <-
  "Code 1 when nondisclosure/attribution-failure language and misconduct/penalty language both appear in the policy text (same clause or elsewhere in the same document)."
summary_df$definition[summary_df$indicator == "detector_mention_any"] <-
  "Code 1 when policy explicitly mentions AI-content detection tools or AI-detection functionality (including Turnitin only when AI-detection function is referenced)."

write.csv(coding, output_coding, row.names = FALSE, na = "")
write.csv(evidence, output_evidence, row.names = FALSE, na = "")
write.csv(summary_df, output_summary, row.names = FALSE, na = "")

summary_md <- summary_df[, c("indicator_label", "display")]
names(summary_md) <- c("Indicator", paste0("All international policies (*n*=", n_total, ")"))
md_lines <- c(
  "# Table",
  "",
  paste0("Table. International policy signals for disclosure, misuse procedures, sanctions, and detector governance (*n* = ", n_total, ")."),
  "",
  "Note. For \"Required disclosure template OR standardized statement language\":",
  "code = 1 only when policy text explicitly requires either a disclosure template/form or standardized statement language.",
  "Note. For \"Faculty procedures for suspected AI misuse\":",
  "code = 1 only when policy text explicitly specifies actions to take when misuse is suspected.",
  "Note. For \"Sanctions for nondisclosure (incl. attribution failure)\":",
  "code = 1 when nondisclosure/attribution-failure and misconduct/penalty language both appear in the same policy text.",
  "Note. For \"Detector mention at all\":",
  "code = 1 only for explicit AI-detection tool/function mentions (not generic plagiarism/text-matching tools).",
  "",
  to_md_table(summary_md)
)
writeLines(md_lines, output_summary_md, useBytes = TRUE)

# Manuscript-ready compact table (fixed row labels + operational definitions).
table_order <- c(
  "required_disclosure_template",
  "faculty_procedure_suspected_ai_misuse",
  "sanctions_for_nondisclosure",
  "detector_mention_any",
  "detector_not_sole_evidence",
  "required_detector_screening"
)
table_labels <- c(
  required_disclosure_template = "Required disclosure template OR standardized statement language",
  faculty_procedure_suspected_ai_misuse = "Faculty procedures for suspected AI misuse",
  sanctions_for_nondisclosure = "Sanctions for nondisclosure",
  detector_mention_any = "Detector mention at all",
  detector_not_sole_evidence = "Detector explicitly \"not sole evidence\"",
  required_detector_screening = "Required detector screening anywhere"
)
table_definitions <- c(
  required_disclosure_template = "A named template/form for AI-use disclosure is required, or standardized statement language is prescribed (beyond a general expectation to disclose).",
  faculty_procedure_suspected_ai_misuse = "Policy provides procedural steps for instructors/staff when undisclosed AI misuse is suspected (e.g., documentation, escalation, review pathway).",
  sanctions_for_nondisclosure = "Policy explicitly treats nondisclosure/misrepresentation as misconduct or specifies penalties/sanctions tied to nondisclosure.",
  detector_mention_any = "Policy explicitly mentions AI-content detectors/detection software in the context of assessment or academic-integrity processes.",
  detector_not_sole_evidence = "Policy explicitly states detector outputs are not dispositive/cannot be used as sole evidence for misconduct findings.",
  required_detector_screening = "Policy requires routine detector screening of submissions (e.g., mandatory AI-detection check prior to submission, marking, or progression)."
)

table_df <- data.frame(
  `Policy signal` = unname(table_labels[table_order]),
  `n (%)` = summary_df$display[match(table_order, summary_df$indicator)],
  `Operational definition (coded 1 when policy explicitly states...)` = unname(table_definitions[table_order]),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
write.csv(table_df, output_table_csv, row.names = FALSE, na = "", quote = TRUE)

none_signal_n <- sum(coding$policy_signal_score == 0, na.rm = TRUE)
none_signal_pct <- round(100 * none_signal_n / n_total, 1)

table_md <- table_df
table_md_lines <- c(
  "# Table",
  "",
  paste0("Table. Additional policy signals in international university AI governance (n = ", n_total, ")."),
  "",
  to_md_table(table_md)
  ,
  "",
  paste0(
    "Note. Indicators are coded from publicly accessible institutional policies for the ",
    n_total,
    " international universities in our sample. Across the six indicators, ",
    none_signal_n,
    "/",
    n_total,
    " (",
    sprintf("%.1f%%", none_signal_pct),
    ") of institutions contain none of these operationalization signals."
  )
)
writeLines(table_md_lines, output_table_md, useBytes = TRUE)

cat("International policy signals coding complete.\n")
cat("Outputs:\n")
cat(" - ", output_coding, "\n", sep = "")
cat(" - ", output_evidence, "\n", sep = "")
cat(" - ", output_summary, "\n", sep = "")
cat(" - ", output_summary_md, "\n", sep = "")
cat(" - ", output_table_csv, "\n", sep = "")
cat(" - ", output_table_md, "\n", sep = "")
cat("Rows coded:", n_total, "\n")
cat("Rows needing manual check:", sum(coding$needs_manual_check), "\n")
