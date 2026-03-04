################################################################################
# Supplemental coding: procedural safeguards in China adopter policies.
# Five Box-1 safeguards plus additional D2/D3/D4-aligned items
# are coded from policy text (markdown source preferred; auxiliary text
# fallback only when source markdown is unavailable).
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
index_file <- normalizePath(file.path(root_dir, "00_source_material/04_policy_markdown/policy_markdown_index.csv"), mustWork = TRUE)
subtype_file <- file.path(root_dir, "30_analysis/main/results/SM_china_policy_subtypes.csv")
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_coding <- file.path(output_dir, "SM_china_safeguards_coding.csv")
output_evidence <- file.path(output_dir, "SM_china_safeguards_evidence.csv")
output_qc <- file.path(output_dir, "SM_china_safeguards_qc_discordance.csv")
output_adjudication <- file.path(output_dir, "SM_china_d1d2_harmonized_adjudication_37.csv")

to_int <- function(x) suppressWarnings(as.integer(trimws(as.character(x))))
norm_space <- function(x) gsub("[[:space:]]+", " ", trimws(gsub("[\r\n]+", " ", x)))

extract_full_policy_text <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return("")
  }
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  anchor <- grep("^## Full Policy Text", lines)
  if (length(anchor) > 0 && anchor[1] < length(lines)) {
    txt <- paste(lines[(anchor[1] + 1):length(lines)], collapse = "\n")
  } else {
    txt <- paste(lines, collapse = "\n")
  }
  trimws(txt)
}

is_informative_policy_text <- function(txt, min_chars = 30L) {
  if (is.na(txt) || !nzchar(trimws(txt))) {
    return(FALSE)
  }
  z <- txt
  z <- gsub("###\\s*Page\\s*[0-9]+", " ", z, perl = TRUE)
  z <- gsub("\\[No extractable text on this page\\]", " ", z, fixed = TRUE)
  z <- gsub("[[:space:][:punct:]]+", "", z, perl = TRUE)
  nchar(z) >= min_chars
}

first_match_info <- function(text, pattern) {
  if (is.na(text) || !nzchar(text)) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  m <- regexpr(pattern, text, perl = TRUE, ignore.case = TRUE)
  if (m[1] == -1) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  start <- as.integer(m[1])
  mlen <- as.integer(attr(m, "match.length"))
  end <- start + mlen - 1L
  keyword <- substr(text, start, end)
  left <- max(1L, start - 110L)
  right <- min(nchar(text), end + 170L)
  excerpt <- norm_space(substr(text, left, right))
  list(hit = 1L, keyword = keyword, excerpt = excerpt)
}

first_match_with_context <- function(text, pattern, context_pattern, window = 140L) {
  if (is.na(text) || !nzchar(text)) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  starts <- gregexpr(pattern, text, perl = TRUE, ignore.case = TRUE)[[1]]
  if (length(starts) == 1 && starts[1] == -1) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  lens <- attr(starts, "match.length")
  for (j in seq_along(starts)) {
    st <- as.integer(starts[j])
    en <- st + as.integer(lens[j]) - 1L
    left <- max(1L, st - window)
    right <- min(nchar(text), en + window)
    local <- substr(text, left, right)
    if (grepl(context_pattern, local, perl = TRUE, ignore.case = TRUE)) {
      keyword <- substr(text, st, en)
      excerpt <- norm_space(local)
      return(list(hit = 1L, keyword = keyword, excerpt = excerpt))
    }
  }
  list(hit = 0L, keyword = "", excerpt = "")
}

first_clause_match <- function(text, clause_fun) {
  if (is.na(text) || !nzchar(text)) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  clauses <- unlist(strsplit(text, "[。；;！？!?\\n]+", perl = TRUE))
  clauses <- clauses[nzchar(trimws(clauses))]
  if (length(clauses) == 0) {
    return(list(hit = 0L, keyword = "", excerpt = ""))
  }
  for (cl in clauses) {
    out <- clause_fun(cl)
    if (isTRUE(out$hit == 1L)) {
      return(list(
        hit = 1L,
        keyword = ifelse(is.null(out$keyword), "", out$keyword),
        excerpt = norm_space(cl)
      ))
    }
  }
  list(hit = 0L, keyword = "", excerpt = "")
}

score_safeguards <- function(text) {
  detector_context_broad <- paste0(
    "(",
    "aigc|ai[- ]?generated|ai\\s*检测|aigc\\s*检测|",
    "检测结果|检测报告|查重|检测系统|智能生成|",
    "artificial intelligence generated content",
    ")"
  )
  detector_context_ai <- paste0(
    "(",
    "aigc|人工智能|ai[- ]?generated|ai\\s*检测|aigc\\s*检测|",
    "智能生成|疑似\\s*aigc|检测结果",
    ")"
  )
  detector_context_disclosure <- paste0(
    "(",
    "aigc|人工智能|artificial\\s+intelligence|generative\\s+ai|",
    "ai\\s+tools?|ai\\s+writing\\s+tools?|ai\\s+technolog(y|ies)|",
    "ai工具|ai辅助工具|ai写作工具|使用ai|ai生成|",
    "ai[- ]?generated|ai\\s*检测|aigc\\s*检测|智能生成|检测结果",
    ")"
  )
  patt_human <- paste0(
    "(",
    "manual review|human review|review panel|",
    "人工复核|人工审核|人工核验|",
    "学术伦理调查|学术不端调查|伦理审查|人工判定",
    ")"
  )
  patt_appeal <- paste0(
    "(",
    "appeal|right to reply|right of reply|",
    "申诉|复议|异议处理|异议渠道|申辩|陈述意见|申诉渠道",
    ")"
  )
  patt_tool <- paste0(
    "(",
    "cnki|知网|维普|turnitin|格子达|",
    "aigc检测系统|检测平台|检测工具|检测系统",
    ")"
  )
  patt_threshold <- paste0(
    "(",
    "[0-9]+\\s*%|阈值|比例|不超过|高于|低于|不得超过|不高于|不低于|<=|>=|≤|≥",
    ")"
  )
  patt_record <- paste0(
    "(",
    "留存|存档|备案|记录|报告单|检测报告|结果保存|归档|留档|日志|版本|version",
    ")"
  )
  patt_not_sole <- paste0(
    "(",
    "not\\s+.*sole\\s+evidence|reference only|auxiliary reference|",
    "仅供参考|辅助参考|仅作参考|",
    "不作为[^。；,，]*(原创性|质量检查|判定)[^。；,，]*依据|",
    "不作为[^。；,，]*(强制性)?[^。；,，]*(评价标准|评判标准)|",
    "不能作为[^。；,，]*(原创性|判定)[^。；,，]*依据|",
    "不得作为[^。；,，]*(原创性|判定)[^。；,，]*依据|",
    "不与[^。；,，]*(通过与否|是否通过)[^。；,，]*挂钩",
    ")"
  )
  patt_limit <- paste0(
    "(",
    "technical limitation|limitations|error rate|false positive|false negative|uncertainty|probabilistic|",
    "hallucination|opaque|black[- ]?box|",
    "reliability[^。；;,.]{0,35}(not|uncertain|limited|insufficient|immature)|",
    "effectiveness[^。；;,.]{0,35}(not|uncertain|limited|insufficient|immature)|",
    "reliability\\s+and\\s+effectiveness[^。；;,.]{0,35}(not|uncertain|limited|insufficient|immature)|",
    "检测效果尚不明确|效果尚不明确|准确性尚不明确|",
    "可靠性[^。；;，,]{0,20}(不足|有限|不高|不稳定|尚不明确|尚未验证)|",
    "有效性[^。；;，,]{0,20}(不足|有限|不高|不稳定|尚不明确|尚未验证)|",
    "可靠性和有效性[^。；;，,]{0,20}(不足|有限|不高|不稳定|尚不明确|尚未验证)|",
    "技术限制|技术局限|局限性|误差|误判|准确率|假阳性|假阴性|不确定性|概率分析|概率模型|",
    "黑箱|不透明|幻觉",
    ")"
  )
  patt_disclosure <- paste0(
    "(",
    "ai\\s*tool\\s*usage\\s*declaration\\s*form|",
    "artificial\\s+intelligence\\s+use\\s+record\\s+form|",
    "artificial\\s+intelligence\\s+technology\\s+use\\s+declaration|",
    "statement\\s+on\\s+the\\s+use\\s+of\\s+ai\\s+tools|",
    "disclosure\\s+and\\s+declaration|",
    "usage\\s+must\\s+be\\s+explicitly\\s+documented|",
    "must\\s+be\\s+declared\\s+in\\s+sections\\s+such\\s+as\\s+notes,\\s*acknowledgments,\\s*references,\\s*or\\s*appendices|",
    "submit\\s+an\\s+artificial\\s+intelligence\\s+technology\\s+use\\s+declaration\\s+before\\s+(their\\s+)?thesis\\s+defense|",
    "ai工具使用情况说明表|ai工具使用声明表|",
    "人工智能使用备案表|人工智能工具使用备案表|",
    "使用情况予以说明|在参考文献、致谢等论文相关部分对使用情况予以说明|",
    "在《[^》]*(说明表|备案表)[^》]*》中明确标注|",
    "论文终稿应附《[^》]*(说明表|备案表)[^》]*》|",
    "明确标注使用的ai工具及其作用",
    ")"
  )
  patt_responsibility <- paste0(
    "(",
    "students?\\s+(shall|must|are\\s+to|should)\\s+.*(responsible|take\\s+responsibility)|",
    "students?\\s+retain\\s+ultimate\\s+responsibility|",
    "author\\s+bears\\s+ultimate\\s+responsibility|",
    "the\\s+author\\s+.*(responsible|responsibility)|",
    "ultimate\\s+responsibility\\s+.*students?|",
    "responsibility\\s+for\\s+(accuracy|originality|integrity)|",
    "文责自负|",
    "学生应对[^。；;]*承担[^。；;]*(最终)?责任|",
    "学生对[^。；;]*承担[^。；;]*(最终)?责任|",
    "学生应[^。；;]*负责|",
    "学生对[^。；;]*负责|",
    "作者[^。；;]*承担[^。；;]*(最终)?责任|",
    "最终责任[^。；;]*由学生",
    ")"
  )
  patt_ai_as_aid <- paste0(
    "(",
    "supplementary\\s+aid|auxiliary\\s+tool|auxiliary\\s+tasks?|assistive\\s+tool|",
    "auxiliary\\s+force|",
    "may\\s+only\\s+be\\s+utilized\\s+for\\s+auxiliary|",
    "students?\\s+may\\s+use\\s+ai\\s+tools?|",
    "in\\s+principle,\\s*students?\\s+may\\s+use\\s+ai\\s+tools?|",
    "scientific\\s+and\\s+reasonable\\s+use\\s+of\\s+ai\\s+tools?|",
    "treated\\s+solely\\s+as\\s+supplementary\\s+aids?|",
    "allowed\\s+under\\s+.*conditions|",
    "可作为[^。；;]*(辅助手段|辅助工具)|",
    "仅可作为[^。；;]*(辅助手段|辅助工具)|",
    "原则上[^。；;]*可[^。；;]*使用[^。；;]*(AI|人工智能|AIGC)|",
    "科学合理使用[^。；;]*(AI工具|人工智能|AIGC)|",
    "(AI|人工智能|AIGC)[^。；;]*(科学合理使用|合理使用)|",
    "可用于[^。；;]*(数据检索|数据处理|头脑风暴|润色|翻译)|",
    "合理使用[^。；;]*(人工智能|AI|AIGC)|",
    "规范使用[^。；;]*(人工智能|AI|AIGC)|",
    "用于支持[^。；;]*(学习|研究)过程|",
    "不能替代[^。；;]*(独立思考|原创性贡献)",
    ")"
  )
  patt_proactive_process <- paste0(
    "(",
    "prompt\\s+history|prompt\\s+log|process\\s+log|draft\\s+history|",
    "separate[^。；;,.]{0,40}(ai-assisted|ai generated|ai-generated)[^。；;,.]{0,40}(original|author)|",
    "(ai|aigc|人工智能)[^。；;，,]{0,26}(过程记录|日志|版本记录|留存|保存)|",
    "(过程记录|日志|版本记录)[^。；;，,]{0,20}(ai|aigc|人工智能)|",
    "区分[^。；;，,]{0,30}(ai|aigc|人工智能)[^。；;，,]{0,30}(生成|辅助)[^。；;，,]{0,30}(原创|本人)|",
    "(答辩|口试)[^。；;，,]{0,24}(核验|询问|真实性|原始材料)[^。；;，,]{0,24}(ai|aigc|人工智能)|",
    "(ai|aigc|人工智能)[^。；;，,]{0,24}(核验|询问|真实性|原始材料)[^。；;，,]{0,24}(答辩|口试)",
    ")"
  )

  hit_human <- first_match_with_context(text, patt_human, detector_context_ai)
  hit_appeal <- first_match_with_context(text, patt_appeal, detector_context_ai)
  hit_not_sole <- first_match_with_context(text, patt_not_sole, detector_context_ai)
  hit_limit <- first_match_with_context(text, patt_limit, detector_context_ai)
  hit_disclosure <- first_match_with_context(text, patt_disclosure, detector_context_disclosure, window = 200L)
  hit_proactive <- first_match_with_context(text, patt_proactive_process, detector_context_disclosure, window = 140L)
  hit_responsibility <- first_match_with_context(text, patt_responsibility, detector_context_disclosure, window = 220L)
  hit_ai_as_aid <- first_match_with_context(text, patt_ai_as_aid, detector_context_disclosure, window = 220L)

  trans_hit <- first_clause_match(text, function(clause) {
    has_context <- grepl(detector_context_broad, clause, perl = TRUE, ignore.case = TRUE)
    has_tool <- grepl(patt_tool, clause, perl = TRUE, ignore.case = TRUE)
    has_threshold <- grepl(patt_threshold, clause, perl = TRUE, ignore.case = TRUE)
    has_record <- grepl(patt_record, clause, perl = TRUE, ignore.case = TRUE)
    is_hit <- has_context && ((has_record && (has_tool || has_threshold)) || (has_tool && has_threshold))
    if (!is_hit) {
      return(list(hit = 0L))
    }
    kw <- if (has_record) {
      regmatches(clause, regexpr(patt_record, clause, perl = TRUE, ignore.case = TRUE))
    } else if (has_tool) {
      regmatches(clause, regexpr(patt_tool, clause, perl = TRUE, ignore.case = TRUE))
    } else {
      regmatches(clause, regexpr(patt_threshold, clause, perl = TRUE, ignore.case = TRUE))
    }
    list(hit = 1L, keyword = kw)
  })
  trans_present <- as.integer(trans_hit$hit == 1L)
  trans_evidence <- if (trans_present == 1L) {
    trans_hit
  } else {
    list(hit = 0L, keyword = "", excerpt = "")
  }

  d1_harmonized_hit <- as.integer(hit_limit$hit == 1L | hit_not_sole$hit == 1L)
  d1_harmonized_keyword <- if (hit_limit$hit == 1L) {
    hit_limit$keyword
  } else if (hit_not_sole$hit == 1L) {
    hit_not_sole$keyword
  } else {
    ""
  }
  d1_harmonized_excerpt <- if (hit_limit$hit == 1L) {
    hit_limit$excerpt
  } else if (hit_not_sole$hit == 1L) {
    hit_not_sole$excerpt
  } else {
    ""
  }

  d2_harmonized_hit <- as.integer(hit_disclosure$hit == 1L | hit_proactive$hit == 1L)
  d2_harmonized_keyword <- if (hit_disclosure$hit == 1L) {
    hit_disclosure$keyword
  } else if (hit_proactive$hit == 1L) {
    hit_proactive$keyword
  } else {
    ""
  }
  d2_harmonized_excerpt <- if (hit_disclosure$hit == 1L) {
    hit_disclosure$excerpt
  } else if (hit_proactive$hit == 1L) {
    hit_proactive$excerpt
  } else {
    ""
  }

  list(
    code = c(
      human_review_requirement = hit_human$hit,
      appeal_right_reply = hit_appeal$hit,
      transparency_records = trans_present,
      detector_not_sole_evidence = hit_not_sole$hit,
      error_limitation_disclosure = hit_limit$hit,
      ai_use_disclosure_statement = hit_disclosure$hit,
      d1_harmonized = d1_harmonized_hit,
      d2_harmonized = d2_harmonized_hit,
      d3_responsibility = hit_responsibility$hit,
      d4_ai_as_aid = hit_ai_as_aid$hit
    ),
    keyword = c(
      human_review_requirement = hit_human$keyword,
      appeal_right_reply = hit_appeal$keyword,
      transparency_records = trans_evidence$keyword,
      detector_not_sole_evidence = hit_not_sole$keyword,
      error_limitation_disclosure = hit_limit$keyword,
      ai_use_disclosure_statement = hit_disclosure$keyword,
      d1_harmonized = d1_harmonized_keyword,
      d2_harmonized = d2_harmonized_keyword,
      d3_responsibility = hit_responsibility$keyword,
      d4_ai_as_aid = hit_ai_as_aid$keyword
    ),
    excerpt = c(
      human_review_requirement = hit_human$excerpt,
      appeal_right_reply = hit_appeal$excerpt,
      transparency_records = trans_evidence$excerpt,
      detector_not_sole_evidence = hit_not_sole$excerpt,
      error_limitation_disclosure = hit_limit$excerpt,
      ai_use_disclosure_statement = hit_disclosure$excerpt,
      d1_harmonized = d1_harmonized_excerpt,
      d2_harmonized = d2_harmonized_excerpt,
      d3_responsibility = hit_responsibility$excerpt,
      d4_ai_as_aid = hit_ai_as_aid$excerpt
    )
  )
}

china <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)
china$check_framework <- to_int(china[["check_framework(0/1)"]])
china$tool_specified <- to_int(china[["tool_specified(0/1)"]])
china$linked_to_pass <- to_int(china[["linked_to_pass(0/1)"]])

governance_ids <- integer(0)
if (file.exists(subtype_file)) {
  subtype <- read.csv(subtype_file, stringsAsFactors = FALSE)
  if (all(c("id", "policy_subtype") %in% names(subtype))) {
    governance_ids <- to_int(subtype$id[subtype$policy_subtype == "AI governance present, no detection"])
    governance_ids <- governance_ids[!is.na(governance_ids)]
  }
}

target_ids <- sort(unique(c(china$id[china$check_framework == 1], governance_ids)))
adopters <- subset(china, id %in% target_ids)
adopters$sample_group <- ifelse(
  adopters$check_framework == 1,
  "mandated_detection",
  ifelse(adopters$id %in% governance_ids, "non_detection_governance", "other")
)

idx <- read.csv(index_file, stringsAsFactors = FALSE)
idx <- subset(
  idx,
  grepl("^00_source_material/01_chinese_original/", source_relpath) &
    nzchar(output_relpath)
)

direct_idx <- subset(idx, dataset_match == "S1" & !is.na(dataset_row_id) & nzchar(dataset_row_id))
direct_idx$dataset_row_id <- to_int(direct_idx$dataset_row_id)
direct_idx <- direct_idx[!is.na(direct_idx$dataset_row_id), c("dataset_row_id", "output_relpath")]
direct_idx <- direct_idx[!duplicated(direct_idx$dataset_row_id), ]
direct_map <- setNames(direct_idx$output_relpath, as.character(direct_idx$dataset_row_id))

# Manual overrides for rows that were not auto-linked in policy_markdown_index.csv.
manual_map <- c(
  "3" = "00_source_material/04_policy_markdown/01_chinese_original/03_【2025届本科生毕业论文5号】关于本科生毕业论文（设计）查重的说明.md",
  "6" = "00_source_material/04_policy_markdown/01_chinese_original/06_关于加强2025年本科毕业论文（设计）管理工作的通知.md",
  "9" = "00_source_material/04_policy_markdown/01_chinese_original/09_关于做好2025届本科毕业设计(论文)检测工作的通知.md",
  "17" = "00_source_material/04_policy_markdown/01_chinese_original/16_关于2024届本科毕业设计（论文）报告提交及查重检测工作的说明.md",
  "28" = "00_source_material/04_policy_markdown/01_chinese_original/27_关于2025年上学期研究生学位授予工作有关事项的通知-研究生院.md",
  "39" = "00_source_material/04_policy_markdown/01_chinese_original/36_【本科生教学】关于在2025届本科毕业设计（论文）中规范AI工具使用的通知.md",
  "44" = "00_source_material/04_policy_markdown/01_chinese_original/40_关于2025年（2021级）本科生毕业设计（论文）盲审工作安排的通知-本科生院 西安电子科技大学.md",
  "58" = "00_source_material/04_policy_markdown/01_chinese_original/52_关于开展2025届本科毕业设计（论文）评阅、学术不端系统检测的通知-VATUU为途教学信息服务平台.md",
  "61" = "00_source_material/04_policy_markdown/01_chinese_original/54_关于做好本科毕业论文（设计）、查重检测及抽检相关工作的通知-中国地质大学本科生院.md",
  "104" = "00_source_material/04_policy_markdown/01_chinese_original/82_AA9B4251DB4F3F7C7456155F3BE_943364B4_2115F.md",
  "118" = "00_source_material/04_policy_markdown/01_chinese_original/92_关于2025届本科毕业论文中后期工作的通知.md",
  "120" = "00_source_material/04_policy_markdown/01_chinese_original/94_关于做好2025届本科毕业设计（论文）查重和AIGC检测工作的通知-教务处.md",
  "122" = "00_source_material/04_policy_markdown/01_chinese_original/95_关于做好2025届本科毕业论文（设计）工作的通知.md"
)

adopters$markdown_relpath <- unname(direct_map[as.character(adopters$id)])
use_manual <- is.na(adopters$markdown_relpath) | !nzchar(adopters$markdown_relpath)
adopters$markdown_relpath[use_manual] <- unname(manual_map[as.character(adopters$id[use_manual])])
adopters$markdown_method <- ifelse(
  as.character(adopters$id) %in% names(direct_map),
  "index_dataset_row_id",
  ifelse(as.character(adopters$id) %in% names(manual_map), "manual_snapshot_match", "unmapped")
)

adopters$markdown_path <- ifelse(
  is.na(adopters$markdown_relpath) | !nzchar(adopters$markdown_relpath),
  "",
  file.path(root_dir, adopters$markdown_relpath)
)
adopters$markdown_exists <- nzchar(adopters$markdown_path) & file.exists(adopters$markdown_path)

adopters$text_markdown <- vapply(adopters$markdown_path, extract_full_policy_text, character(1))
adopters$text_aux <- paste(
  ifelse(is.na(adopters$threshold), "", adopters$threshold),
  ifelse(is.na(adopters$policy_summary), "", adopters$policy_summary),
  sep = "\n"
)
adopters$text_markdown_available <- adopters$markdown_exists &
  vapply(adopters$text_markdown, is_informative_policy_text, logical(1))
adopters$text_for_coding <- ifelse(adopters$text_markdown_available, adopters$text_markdown, adopters$text_aux)
adopters$text_source_used <- ifelse(adopters$text_markdown_available, "policy_markdown", "threshold_plus_policy_summary")

item_names <- c(
  "human_review_requirement",
  "appeal_right_reply",
  "transparency_records",
  "detector_not_sole_evidence",
  "error_limitation_disclosure",
  "ai_use_disclosure_statement",
  "d1_harmonized",
  "d2_harmonized",
  "d3_responsibility",
  "d4_ai_as_aid"
)
harmonized_items <- c("d1_harmonized", "d2_harmonized")
coverage_items <- c(
  "human_review_requirement",
  "appeal_right_reply",
  "transparency_records",
  "detector_not_sole_evidence",
  "error_limitation_disclosure"
)

final_code <- matrix(0L, nrow = nrow(adopters), ncol = length(item_names))
colnames(final_code) <- item_names
md_code <- matrix(NA_integer_, nrow = nrow(adopters), ncol = length(item_names))
colnames(md_code) <- item_names
aux_code <- matrix(NA_integer_, nrow = nrow(adopters), ncol = length(item_names))
colnames(aux_code) <- item_names

evidence_keyword <- matrix("", nrow = nrow(adopters), ncol = length(item_names))
colnames(evidence_keyword) <- item_names
evidence_excerpt <- matrix("", nrow = nrow(adopters), ncol = length(item_names))
colnames(evidence_excerpt) <- item_names
discordance <- matrix(0L, nrow = nrow(adopters), ncol = length(item_names))
colnames(discordance) <- item_names
manual_override_applied <- matrix(0L, nrow = nrow(adopters), ncol = length(harmonized_items))
colnames(manual_override_applied) <- harmonized_items

# Manual adjudication overrides for harmonized D1/D2 edge cases.
manual_override_map <- list(
  "46" = list(
    d1_harmonized = list(
      value = 1L,
      reason = "Fallback summary indicates AIGC detector reliability/effectiveness is limited in first rollout; outputs are reference-only."
    ),
    d2_harmonized = NULL
  )
)

for (i in seq_len(nrow(adopters))) {
  s_md <- score_safeguards(adopters$text_markdown[i])
  s_aux <- score_safeguards(adopters$text_aux[i])
  s_final <- if (adopters$text_markdown_available[i]) s_md else s_aux

  md_code[i, ] <- s_md$code[item_names]
  aux_code[i, ] <- s_aux$code[item_names]
  final_code[i, ] <- s_final$code[item_names]
  evidence_keyword[i, ] <- s_final$keyword[item_names]
  evidence_excerpt[i, ] <- s_final$excerpt[item_names]

  row_id <- as.character(adopters$id[i])
  if (row_id %in% names(manual_override_map)) {
    ov <- manual_override_map[[row_id]]
    for (hn in harmonized_items) {
      if (!is.null(ov[[hn]]) && !is.null(ov[[hn]]$value) && !is.na(ov[[hn]]$value)) {
        final_code[i, hn] <- as.integer(ov[[hn]]$value)
        evidence_keyword[i, hn] <- "manual_override"
        evidence_excerpt[i, hn] <- ov[[hn]]$reason
        manual_override_applied[i, hn] <- 1L
      }
    }
  }

  if (adopters$text_markdown_available[i]) {
    discordance[i, ] <- as.integer(s_md$code[item_names] != s_aux$code[item_names])
  }
}

coding <- adopters[, c(
  "id",
  "univ_name_cn",
  "univ_name_en",
  "Inst_type",
  "check_framework",
  "sample_group",
  "policy_scope",
  "tool_specified",
  "linked_to_pass",
  "source_url",
  "markdown_relpath",
  "markdown_method",
  "text_source_used"
)]

for (nm in item_names) {
  coding[[nm]] <- final_code[, nm]
}
default_basis <- ifelse(adopters$text_markdown_available, "markdown_direct", "fallback_summary")
coding$d1_harmonized_basis <- ifelse(
  manual_override_applied[, "d1_harmonized"] == 1L,
  "manual_override",
  default_basis
)
coding$d2_harmonized_basis <- ifelse(
  manual_override_applied[, "d2_harmonized"] == 1L,
  "manual_override",
  default_basis
)
coding$d1_harmonized_evidence <- evidence_excerpt[, "d1_harmonized"]
coding$d2_harmonized_evidence <- evidence_excerpt[, "d2_harmonized"]
coding$safeguard_coverage_score <- rowSums(coding[, coverage_items], na.rm = TRUE)
coding$any_md_aux_discordance <- rowSums(discordance, na.rm = TRUE)
coding$needs_manual_check <- as.integer(coding$markdown_method == "unmapped" | coding$any_md_aux_discordance > 0)

evidence_rows <- vector("list", nrow(adopters) * length(item_names))
k <- 0L
for (i in seq_len(nrow(adopters))) {
  for (nm in item_names) {
    k <- k + 1L
    evidence_rows[[k]] <- data.frame(
      id = adopters$id[i],
      univ_name_en = adopters$univ_name_en[i],
      item = nm,
      code_final = final_code[i, nm],
      code_markdown = md_code[i, nm],
      code_aux = aux_code[i, nm],
      md_aux_discordant = discordance[i, nm],
      text_source_used = adopters$text_source_used[i],
      evidence_keyword = evidence_keyword[i, nm],
      evidence_excerpt = evidence_excerpt[i, nm],
      stringsAsFactors = FALSE
    )
  }
}
evidence <- do.call(rbind, evidence_rows)
evidence$harmonized_basis <- ""
map_d1_basis <- setNames(coding$d1_harmonized_basis, as.character(coding$id))
map_d2_basis <- setNames(coding$d2_harmonized_basis, as.character(coding$id))
is_d1 <- evidence$item == "d1_harmonized"
is_d2 <- evidence$item == "d2_harmonized"
evidence$harmonized_basis[is_d1] <- unname(map_d1_basis[as.character(evidence$id[is_d1])])
evidence$harmonized_basis[is_d2] <- unname(map_d2_basis[as.character(evidence$id[is_d2])])
qc <- subset(evidence, md_aux_discordant == 1)

coding <- coding[order(-coding$linked_to_pass, -coding$safeguard_coverage_score, coding$univ_name_en), ]
row.names(coding) <- NULL

adjudication <- coding[order(coding$id), c(
  "id",
  "univ_name_cn",
  "univ_name_en",
  "sample_group",
  "source_url",
  "markdown_relpath",
  "text_source_used",
  "d1_harmonized",
  "d1_harmonized_basis",
  "d1_harmonized_evidence",
  "d2_harmonized",
  "d2_harmonized_basis",
  "d2_harmonized_evidence"
)]
row.names(adjudication) <- NULL

write.csv(coding, output_coding, row.names = FALSE, na = "")
write.csv(evidence, output_evidence, row.names = FALSE, na = "")
write.csv(qc, output_qc, row.names = FALSE, na = "")
write.csv(adjudication, output_adjudication, row.names = FALSE, na = "")

cat("Safeguard coding complete.\n")
cat("Outputs:\n")
cat(" - ", output_coding, "\n", sep = "")
cat(" - ", output_evidence, "\n", sep = "")
cat(" - ", output_qc, "\n", sep = "")
cat(" - ", output_adjudication, "\n", sep = "")
cat("Policies coded:", nrow(coding), "\n")
cat(" - Mandated detection:", sum(coding$sample_group == "mandated_detection"), "\n")
cat(" - Non-detection governance:", sum(coding$sample_group == "non_detection_governance"), "\n")
cat("Markdown-backed rows:", sum(adopters$text_markdown_available), "\n")
cat("Rows needing manual check:", sum(coding$needs_manual_check), "\n")
cat("Manual overrides (harmonized D1/D2):", sum(manual_override_applied), "\n")
