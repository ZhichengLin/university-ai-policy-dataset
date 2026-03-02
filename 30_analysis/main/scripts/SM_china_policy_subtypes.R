################################################################################
# Supplemental coding: China policy subtypes for adoption/non-adoption
# Four categories:
# 1) Mandated detection
# 2) Detection mentioned, not mandated
# 3) AI governance present, no detection
# 4) No AI policy found in corpus
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

output_file <- file.path(output_dir, "SM_china_policy_subtypes.csv")
output_counts <- file.path(output_dir, "SM_china_policy_subtypes_counts.csv")
output_validation <- file.path(output_dir, "SM_china_nonadopter_policy_validation.csv")

to_int <- function(x) suppressWarnings(as.integer(trimws(as.character(x))))
norm_space <- function(x) gsub("[[:space:]]+", " ", trimws(gsub("[\r\n]+", " ", x)))

china <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)
china$check_framework <- to_int(china[["check_framework(0/1)"]])
china$tool_specified <- to_int(china[["tool_specified(0/1)"]])
china$linked_to_pass <- to_int(china[["linked_to_pass(0/1)"]])
china$source_url <- ifelse(is.na(china$source_url), "", trimws(china$source_url))
china$policy_summary <- ifelse(is.na(china$policy_summary), "", china$policy_summary)
china$threshold <- ifelse(is.na(china$threshold), "", china$threshold)
china$text_blob <- paste(china$policy_summary, china$threshold, sep = "\n")

has_policy_evidence <- nzchar(china$source_url) | nzchar(trimws(china$policy_summary)) | nzchar(trimws(china$threshold))

txt_low <- tolower(china$text_blob)
has_detection_mention <- grepl(
  paste(
    "aigc\\s*检测",
    "ai\\s*检测",
    "aigc\\s*(率|比例|score|result)",
    "ai[- ]generated[^。\\n]{0,20}(detection|rate|ratio|score|result)",
    "(检测|detect|detection)[^。\\n]{0,20}(aigc|ai[- ]generated|ai\\s*content|人工智能|生成式人工智能)",
    "ai\\s*content\\s*detection",
    "智能生成[^。\\n]{0,20}(检测|识别)",
    sep = "|"
  ),
  txt_low,
  perl = TRUE
)
has_ai_governance <- grepl(
  "aigc|artificial intelligence|\\bai\\b|生成式|人工智能|伦理|ethic|disclosure|declaration|responsible use|规范使用|学术诚信|integrity",
  txt_low,
  perl = TRUE
)

subtype <- rep("No AI policy found in corpus", nrow(china))
subtype[china$check_framework == 1] <- "Mandated detection"

is_nonadopter <- china$check_framework == 0
subtype[is_nonadopter & has_policy_evidence & has_ai_governance & !has_detection_mention] <- "AI governance present, no detection"
subtype[is_nonadopter & has_policy_evidence & has_detection_mention] <- "Detection mentioned, not mandated"

# Accuracy safeguard: manual confirmation for all non-adopters with policy evidence
# based on current curated policy summaries/coding rationale.
manual_governance_ids <- c(4, 5, 23, 24, 26, 62, 72, 92)
manual_detection_ids <- integer(0)

subtype[china$id %in% manual_governance_ids & is_nonadopter] <- "AI governance present, no detection"
subtype[china$id %in% manual_detection_ids & is_nonadopter] <- "Detection mentioned, not mandated"

subtype_method <- ifelse(
  china$check_framework == 1,
  "direct_check_framework",
  ifelse(china$id %in% c(manual_governance_ids, manual_detection_ids), "manual_validated_nonadopter", "rule_based_nonadopter")
)

out <- china[, c(
  "id",
  "univ_name_cn",
  "univ_name_en",
  "Inst_type",
  "check_framework",
  "tool_specified",
  "linked_to_pass",
  "source_url"
)]
out$policy_evidence_available <- as.integer(has_policy_evidence)
out$policy_subtype <- subtype
out$subtype_method <- subtype_method
out$policy_summary_excerpt <- substr(norm_space(china$policy_summary), 1, 280)

subtype_levels <- c(
  "Mandated detection",
  "Detection mentioned, not mandated",
  "AI governance present, no detection",
  "No AI policy found in corpus"
)
out$policy_subtype <- factor(out$policy_subtype, levels = subtype_levels)
out <- out[order(out$id), ]

counts <- data.frame(
  policy_subtype = subtype_levels,
  n = as.integer(table(factor(out$policy_subtype, levels = subtype_levels))),
  stringsAsFactors = FALSE
)
counts$share_pct <- round(100 * counts$n / nrow(out), 2)
row.names(counts) <- NULL

out$policy_subtype <- as.character(out$policy_subtype)

validation <- subset(out, check_framework == 0 & policy_evidence_available == 1)
validation <- validation[order(validation$id), c(
  "id",
  "univ_name_cn",
  "univ_name_en",
  "policy_subtype",
  "subtype_method",
  "policy_summary_excerpt",
  "source_url"
)]

write.csv(out, output_file, row.names = FALSE, na = "")
write.csv(counts, output_counts, row.names = FALSE, na = "")
write.csv(validation, output_validation, row.names = FALSE, na = "")

cat("China policy subtype coding complete.\n")
cat("Outputs:\n")
cat(" - ", output_file, "\n", sep = "")
cat(" - ", output_counts, "\n", sep = "")
cat(" - ", output_validation, "\n", sep = "")
cat("Subtype counts:\n")
print(counts)
