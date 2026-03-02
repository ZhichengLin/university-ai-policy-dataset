################################################################################
# Supplemental Table: China AIGC threshold rule inventory (full table)
# One row per extracted threshold rule among adopter institutions, AIGC-only.
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
output_file_aigc <- file.path(output_dir, "SM_china_aigc_threshold_rule_inventory_full.csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

china <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

to_int <- function(x) suppressWarnings(as.integer(trimws(as.character(x))))
norm_space <- function(x) gsub("[[:space:]]+", " ", trimws(x))

china$check_framework <- to_int(china[["check_framework(0/1)"]])
china$tool_specified <- to_int(china[["tool_specified(0/1)"]])
china$linked_to_pass <- to_int(china[["linked_to_pass(0/1)"]])
china$threshold <- ifelse(is.na(china$threshold), "", china$threshold)

adopters <- subset(china, check_framework == 1)

split_segments <- function(text) {
  text <- gsub("[\r\n]+", " ", text)
  segments <- unlist(strsplit(text, "[;；。!?]+", perl = TRUE))
  segments <- norm_space(segments)
  segments[nzchar(segments)]
}

metric_type_from <- function(text_low) {
  if (grepl("aigc|ai-generated|ai generated|full-text|ai content|人工智能生成|生成式", text_low, perl = TRUE)) {
    return("AIGC ratio")
  }
  if (grepl("similarity|duplicate|plagiarism|重复率|查重|相似", text_low, perl = TRUE)) {
    return("Similarity ratio")
  }
  "Other/unspecified"
}

closest_distance <- function(text_low, idx, pattern) {
  loc <- gregexpr(pattern, text_low, perl = TRUE)[[1]]
  if (length(loc) == 1 && loc[1] == -1) {
    return(Inf)
  }
  min(abs(loc - idx))
}

extract_clause <- function(seg_low, hit_start, hit_end) {
  punct <- gregexpr("[,;，；。]", seg_low, perl = TRUE)[[1]]
  if (length(punct) == 1 && punct[1] == -1) {
    return(seg_low)
  }
  left <- max(c(0, punct[punct < hit_start])) + 1
  right_candidates <- punct[punct > hit_end]
  right <- if (length(right_candidates) == 0) nchar(seg_low) else min(right_candidates) - 1
  substr(seg_low, left, right)
}

has_aigc_keyword <- function(text_low) {
  grepl("aigc|ai-generated|ai generated|ai content|full-text|人工智能生成|生成式|aigc检测|ai检测", text_low, perl = TRUE)
}

has_similarity_keyword <- function(text_low) {
  grepl("similarity|duplicate|plagiarism|重复率|查重|相似", text_low, perl = TRUE)
}

closest_anchor <- function(text_low, hit_start, pattern, side = c("left", "right")) {
  side <- match.arg(side)
  loc <- gregexpr(pattern, text_low, perl = TRUE)[[1]]
  if (length(loc) == 1 && loc[1] == -1) {
    return(Inf)
  }
  if (side == "left") {
    loc <- loc[loc <= hit_start]
    if (length(loc) == 0) {
      return(Inf)
    }
    return(hit_start - max(loc))
  }
  loc <- loc[loc >= hit_start]
  if (length(loc) == 0) {
    return(Inf)
  }
  min(loc - hit_start)
}

is_aigc_score_threshold_hit <- function(seg_low, hit_start, hit_end) {
  clause <- norm_space(extract_clause(seg_low, hit_start, hit_end))

  if (grepl("spot check|random check|sampling|coverage|抽检|全覆盖|全部检测|检测率", clause, perl = TRUE)) {
    return(FALSE)
  }
  if (!has_aigc_keyword(clause)) {
    return(FALSE)
  }
  grepl(
    "ratio|proportion|value|result|content|threshold|阈值|检测结果|检测值|占比|比例|not exceed|exceed|below|above|<=|>=|≤|≥|不得超过|不高于|低于|高于|pass|failing|disqualif",
    clause,
    perl = TRUE
  )
}

metric_type_for_hit <- function(seg_low, hit_start, hit_end) {
  clause <- extract_clause(seg_low, hit_start, hit_end)
  clause_aigc <- has_aigc_keyword(clause)
  clause_sim <- has_similarity_keyword(clause)

  # First try local-left context: the immediate metric phrase before the %.
  left_ctx <- substr(seg_low, max(1, hit_start - 70), hit_start)
  left_aigc <- has_aigc_keyword(left_ctx)
  left_sim <- has_similarity_keyword(left_ctx)
  if (left_aigc && !left_sim) {
    return("AIGC ratio")
  }
  if (left_sim && !left_aigc) {
    return("Similarity ratio")
  }

  # When both cues appear in the same clause, prefer the nearest preceding cue.
  if (clause_aigc && clause_sim) {
    left_dist_aigc <- closest_anchor(seg_low, hit_start, "aigc|ai-generated|ai generated|full-text|ai content|人工智能生成|生成式|aigc检测|ai检测", "left")
    left_dist_sim <- closest_anchor(seg_low, hit_start, "similarity|duplicate|plagiarism|重复率|查重|相似", "left")
    if (is.finite(left_dist_aigc) || is.finite(left_dist_sim)) {
      if (left_dist_aigc < left_dist_sim) {
        return("AIGC ratio")
      }
      if (left_dist_sim < left_dist_aigc) {
        return("Similarity ratio")
      }
    }
  }

  if (clause_aigc && !clause_sim) {
    return("AIGC ratio")
  }
  if (clause_sim && !clause_aigc) {
    return("Similarity ratio")
  }

  dist_aigc <- closest_distance(
    seg_low,
    hit_start,
    "aigc|ai-generated|ai generated|full-text|ai content|人工智能生成|生成式|aigc检测|ai检测"
  )
  dist_sim <- closest_distance(
    seg_low,
    hit_start,
    "similarity|duplicate|plagiarism|重复率|查重|相似"
  )

  if (is.finite(dist_aigc) && !is.finite(dist_sim)) {
    return("AIGC ratio")
  }
  if (is.finite(dist_sim) && !is.finite(dist_aigc)) {
    return("Similarity ratio")
  }
  if (is.finite(dist_aigc) && is.finite(dist_sim)) {
    if (dist_aigc < dist_sim) {
      return("AIGC ratio")
    }
    if (dist_sim < dist_aigc) {
      return("Similarity ratio")
    }
  }
  metric_type_from(seg_low)
}

excerpt_around_hit <- function(seg_clean, start_idx, end_idx, pad = 120) {
  left <- max(1, start_idx - pad)
  right <- min(nchar(seg_clean), end_idx + pad)
  norm_space(substr(seg_clean, left, right))
}

decision_point_from <- function(text_low) {
  if (grepl("defen|答辩", text_low, perl = TRUE)) {
    return("Defense eligibility")
  }
  if (grepl("degree|conferral|学位", text_low, perl = TRUE)) {
    return("Degree conferral")
  }
  if (grepl("graduat|毕业|pass|通过|disqualif|not proceed|不予|不得", text_low, perl = TRUE)) {
    return("Graduation or thesis progression")
  }
  if (grepl("submission|提交", text_low, perl = TRUE)) {
    return("Submission screening")
  }
  "Unspecified"
}

remediation_from <- function(text_low) {
  if (grepl("revise|resubmit|manual review|investigation|整改|修改|重做|复核|评审|再次检测|重新检测|延期", text_low, perl = TRUE)) {
    return("Yes")
  }
  if (grepl("not allowed|disqualif|不允许|不得进入|取消", text_low, perl = TRUE)) {
    return("No")
  }
  "Not stated"
}

operator_from <- function(seg_low, start_idx, end_idx) {
  left <- substr(seg_low, max(1, start_idx - 28), start_idx)
  right <- substr(seg_low, end_idx, min(nchar(seg_low), end_idx + 28))
  win <- paste(left, right)

  if (grepl("<=|≤|not exceed|no more than|不得超过|不超过|不高于|at most|不大于", win, perl = TRUE)) {
    return("<=")
  }
  if (grepl(">=|≥|not less than|at least|不少于|不低于|不得低于", win, perl = TRUE)) {
    return(">=")
  }
  if (grepl("less than|below|低于|小于", win, perl = TRUE)) {
    return("<")
  }
  if (grepl("greater than|above|exceed|超过|高于|大于", win, perl = TRUE)) {
    return(">")
  }
  "Value stated (operator unspecified)"
}

extract_policy_rows <- function(row) {
  threshold_raw <- ifelse(is.na(row$threshold), "", row$threshold)
  summary_raw <- ifelse(is.na(row$policy_summary), "", row$policy_summary)
  text_raw <- if (nzchar(trimws(threshold_raw))) threshold_raw else summary_raw
  text_raw <- gsub("[\r\n]+", " ", text_raw)
  text_low <- tolower(text_raw)

  has_threshold_keyword <- grepl(
    "threshold|阈值|比例|ratio|proportion|不得超过|不高于|不低于|低于|高于|<=|>=|≤|≥|%",
    text_low,
    perl = TRUE
  )

  segments <- split_segments(text_raw)
  if (length(segments) == 0) {
    segments <- norm_space(text_raw)
  }

  rows <- list()

  for (seg in segments) {
    seg_clean <- norm_space(seg)
    seg_low <- tolower(seg_clean)
    m <- gregexpr("[0-9]+(?:\\.[0-9]+)?[[:space:]]*%", seg_low, perl = TRUE)[[1]]
    if (length(m) == 1 && m[1] == -1) {
      next
    }
    hit_len <- attr(m, "match.length")
    hits <- regmatches(seg_low, list(m))[[1]]

    for (k in seq_along(hits)) {
      val <- suppressWarnings(as.numeric(gsub("%", "", gsub("[[:space:]]", "", hits[k]))))
      op <- operator_from(seg_low, m[k], m[k] + hit_len[k] - 1)
      metric_hit <- metric_type_for_hit(seg_low, m[k], m[k] + hit_len[k] - 1)
      if (metric_hit == "AIGC ratio" && !is_aigc_score_threshold_hit(seg_low, m[k], m[k] + hit_len[k] - 1)) {
        next
      }
      rows[[length(rows) + 1]] <- data.frame(
        id = row$id,
        univ_name_cn = row$univ_name_cn,
        univ_name_en = row$univ_name_en,
        Inst_type = row$Inst_type,
        tool_name = row$tool_name,
        policy_scope = row$policy_scope,
        tool_specified = row$tool_specified,
        linked_to_pass = row$linked_to_pass,
        threshold_value_pct = val,
        operator = op,
        metric_type = metric_hit,
        decision_point = decision_point_from(seg_low),
        remediation = remediation_from(seg_low),
        threshold_text_excerpt = excerpt_around_hit(seg_clean, m[k], m[k] + hit_len[k] - 1),
        source_url = row$source_url,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    rows[[1]] <- data.frame(
      id = row$id,
      univ_name_cn = row$univ_name_cn,
      univ_name_en = row$univ_name_en,
      Inst_type = row$Inst_type,
      tool_name = row$tool_name,
      policy_scope = row$policy_scope,
      tool_specified = row$tool_specified,
      linked_to_pass = row$linked_to_pass,
      threshold_value_pct = NA_real_,
      operator = ifelse(has_threshold_keyword, "Not numerically specified", "No threshold value in threshold field"),
      metric_type = metric_type_from(text_low),
      decision_point = decision_point_from(text_low),
      remediation = remediation_from(text_low),
      threshold_text_excerpt = substr(norm_space(text_raw), 1, 320),
      source_url = row$source_url,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  dedup_cols <- c("threshold_value_pct", "operator", "metric_type", "decision_point", "remediation")
  out <- out[!duplicated(out[, dedup_cols]), , drop = FALSE]
  out$threshold_rule_index <- seq_len(nrow(out))
  out
}

inventory_list <- lapply(seq_len(nrow(adopters)), function(i) extract_policy_rows(adopters[i, , drop = FALSE]))
inventory <- do.call(rbind, inventory_list)
inventory$has_numeric_threshold <- ifelse(is.na(inventory$threshold_value_pct), 0L, 1L)

inventory <- inventory[, c(
  "id",
  "univ_name_cn",
  "univ_name_en",
  "Inst_type",
  "policy_scope",
  "tool_name",
  "tool_specified",
  "linked_to_pass",
  "threshold_rule_index",
  "has_numeric_threshold",
  "metric_type",
  "operator",
  "threshold_value_pct",
  "decision_point",
  "remediation",
  "threshold_text_excerpt",
  "source_url"
)]

inventory <- inventory[order(inventory$id, inventory$threshold_rule_index), ]
row.names(inventory) <- NULL

aigc_inventory <- subset(inventory, metric_type == "AIGC ratio")

# Keep one row per university per identical AIGC percentage.
# Prefer rows with explicit operators and more informative decision/remediation tags.
aigc_inventory$operator_rank <- ifelse(
  aigc_inventory$operator %in% c("<=", "<", ">=", ">"),
  2L,
  ifelse(aigc_inventory$operator == "Not numerically specified", 1L, 0L)
)
aigc_inventory$decision_rank <- ifelse(aigc_inventory$decision_point == "Unspecified", 0L, 1L)
aigc_inventory$remediation_rank <- ifelse(aigc_inventory$remediation == "Not stated", 0L, 1L)
aigc_inventory$priority_score <- 4L * aigc_inventory$operator_rank +
  2L * aigc_inventory$decision_rank +
  aigc_inventory$remediation_rank

num_rows <- subset(aigc_inventory, !is.na(threshold_value_pct))
num_rows <- num_rows[order(
  num_rows$id,
  num_rows$threshold_value_pct,
  -num_rows$priority_score,
  num_rows$threshold_rule_index
), ]
num_rows <- num_rows[!duplicated(num_rows[, c("id", "threshold_value_pct")]), ]

na_rows <- subset(aigc_inventory, is.na(threshold_value_pct))
if (nrow(na_rows) > 0) {
  na_rows <- na_rows[order(na_rows$id, -na_rows$priority_score, na_rows$threshold_rule_index), ]
  na_rows <- na_rows[!duplicated(na_rows$id), ]
}

aigc_inventory <- rbind(num_rows, na_rows)
aigc_inventory <- aigc_inventory[order(aigc_inventory$id, aigc_inventory$threshold_rule_index), ]
aigc_inventory$threshold_rule_index <- ave(
  aigc_inventory$id,
  aigc_inventory$id,
  FUN = seq_along
)
aigc_inventory <- aigc_inventory[, setdiff(names(aigc_inventory), c(
  "operator_rank",
  "decision_rank",
  "remediation_rank",
  "priority_score"
))]

write.csv(aigc_inventory, output_file_aigc, row.names = FALSE, na = "")

cat("Supplemental threshold table complete.\n")
cat("Output:\n")
cat(" - ", output_file_aigc, "\n", sep = "")
cat("Adopter universities input:", nrow(adopters), "\n")
cat("AIGC-only rows:", nrow(aigc_inventory), "\n")
cat("AIGC universities covered:", length(unique(aigc_inventory$id)), "\n")
