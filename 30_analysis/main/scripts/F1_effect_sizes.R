################################################################################
# F1 Effect Sizes: China adoption estimates with uncertainty
# Produces manuscript-facing outputs for Supplemental Materials F1 extension
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

package_list <- c("ggplot2", "dplyr", "lemon", "svglite", "scales")

load_packages <- function(pkgs, install_missing = FALSE) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0 && install_missing) {
    install.packages(missing_pkgs)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

load_packages(package_list, install_missing = FALSE)

script_dir <- resolve_script_dir()
root_dir <- normalizePath(file.path(script_dir, "../../.."), mustWork = TRUE)
input_file <- normalizePath(file.path(root_dir, "20_coding/21_store/S1_china_analysis.csv"), mustWork = TRUE)
output_dir <- normalizePath(file.path(root_dir, "30_analysis/main/results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

china <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

required_cols <- c("Inst_type", "check_framework(0/1)", "tool_specified(0/1)", "linked_to_pass(0/1)")
missing_cols <- setdiff(required_cols, names(china))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

to_int <- function(x) as.integer(as.character(x))

china$check_framework <- to_int(china[["check_framework(0/1)"]])
china$tool_specified <- to_int(china[["tool_specified(0/1)"]])
china$linked_to_pass <- to_int(china[["linked_to_pass(0/1)"]])

validate_binary <- function(x, name) {
  vals <- sort(unique(x[!is.na(x)]))
  if (!all(vals %in% c(0L, 1L))) {
    stop("Column ", name, " has non-binary values: ", paste(vals, collapse = ", "))
  }
}

validate_binary(china$check_framework, "check_framework(0/1)")
validate_binary(china$tool_specified, "tool_specified(0/1)")
validate_binary(china$linked_to_pass, "linked_to_pass(0/1)")

if (any(is.na(china$check_framework)) || any(is.na(china$tool_specified)) || any(is.na(china$linked_to_pass))) {
  stop("Binary coding columns contain missing values; resolve missingness before inferential summaries.")
}

exact_ci <- function(success, total) {
  bt <- binom.test(success, total)
  c(low = unname(bt$conf.int[1]), high = unname(bt$conf.int[2]))
}

rate_row <- function(metric_id, label, success, total, denominator_scope) {
  ci <- exact_ci(success, total)
  data.frame(
    metric_id = metric_id,
    label = label,
    n_success = success,
    n_total = total,
    proportion = success / total,
    ci_low = ci["low"],
    ci_high = ci["high"],
    denominator_scope = denominator_scope,
    stringsAsFactors = FALSE
  )
}

n_total <- nrow(china)
n_adopters <- sum(china$check_framework == 1L)
n_linked_among_adopters <- sum(china$check_framework == 1L & china$linked_to_pass == 1L)
n_tool_among_adopters <- sum(china$check_framework == 1L & china$tool_specified == 1L)

headline_rate_cis <- bind_rows(
  rate_row(
    metric_id = "check_framework_rate",
    label = "Formal check framework (all institutions)",
    success = n_adopters,
    total = n_total,
    denominator_scope = "all institutions"
  ),
  rate_row(
    metric_id = "linked_to_pass_among_adopters",
    label = "Linked-to-pass among adopters",
    success = n_linked_among_adopters,
    total = n_adopters,
    denominator_scope = "adopters only"
  ),
  rate_row(
    metric_id = "tool_specified_among_adopters",
    label = "Tool specified among adopters",
    success = n_tool_among_adopters,
    total = n_adopters,
    denominator_scope = "adopters only"
  )
) %>%
  mutate(
    proportion_pct = 100 * proportion,
    ci_low_pct = 100 * ci_low,
    ci_high_pct = 100 * ci_high
  )

inst_type_lower <- tolower(trimws(china$Inst_type))
sci_eng_flag <- grepl("science and engineering", inst_type_lower, fixed = TRUE)
comprehensive_flag <- grepl("comprehensive", inst_type_lower, fixed = TRUE)

if (!any(sci_eng_flag)) stop("No science-and-engineering institutions found in Inst_type.")
if (!any(comprehensive_flag)) stop("No comprehensive institutions found in Inst_type.")

contrast_effects <- function(df, exposure_flag, contrast_id, contrast_label) {
  a <- sum(exposure_flag & df$check_framework == 1L)
  b <- sum(exposure_flag & df$check_framework == 0L)
  c <- sum(!exposure_flag & df$check_framework == 1L)
  d <- sum(!exposure_flag & df$check_framework == 0L)

  risk_exposed <- a / (a + b)
  risk_reference <- c / (c + d)

  correction_applied <- any(c(a, b, c, d) == 0L)
  a2 <- a
  b2 <- b
  c2 <- c
  d2 <- d

  if (correction_applied) {
    a2 <- a + 0.5
    b2 <- b + 0.5
    c2 <- c + 0.5
    d2 <- d + 0.5
  }

  rr <- (a2 / (a2 + b2)) / (c2 / (c2 + d2))
  se_log_rr <- sqrt((1 / a2) - (1 / (a2 + b2)) + (1 / c2) - (1 / (c2 + d2)))
  rr_ci <- exp(log(rr) + c(-1, 1) * 1.96 * se_log_rr)

  ft <- fisher.test(matrix(c(a, b, c, d), nrow = 2, byrow = TRUE))

  data.frame(
    contrast_id = contrast_id,
    contrast_label = contrast_label,
    n_exposed = a + b,
    n_reference = c + d,
    adopters_exposed = a,
    non_adopters_exposed = b,
    adopters_reference = c,
    non_adopters_reference = d,
    risk_exposed = risk_exposed,
    risk_reference = risk_reference,
    risk_ratio = rr,
    risk_ratio_ci_low = rr_ci[1],
    risk_ratio_ci_high = rr_ci[2],
    odds_ratio = unname(ft$estimate),
    odds_ratio_ci_low = unname(ft$conf.int[1]),
    odds_ratio_ci_high = unname(ft$conf.int[2]),
    fisher_exact_p = ft$p.value,
    zero_cell_correction = correction_applied,
    stringsAsFactors = FALSE
  )
}

type_effect_sizes <- bind_rows(
  contrast_effects(
    china, sci_eng_flag,
    contrast_id = "sci_eng_vs_others",
    contrast_label = "Science and engineering vs all other institution types"
  ),
  contrast_effects(
    china, comprehensive_flag,
    contrast_id = "comprehensive_vs_others",
    contrast_label = "Comprehensive vs all other institution types"
  )
)

type_adoption <- china %>%
  group_by(Inst_type) %>%
  summarise(
    n_total = n(),
    n_adopters = sum(check_framework == 1L),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    proportion = n_adopters / n_total,
    ci_low = exact_ci(n_adopters, n_total)["low"],
    ci_high = exact_ci(n_adopters, n_total)["high"]
  ) %>%
  ungroup() %>%
  mutate(
    proportion_pct = 100 * proportion,
    ci_low_pct = 100 * ci_low,
    ci_high_pct = 100 * ci_high
  ) %>%
  arrange(desc(proportion), desc(n_total), Inst_type)

china$enforcement_depth <- ifelse(
  china$check_framework == 0L,
  "No framework",
  ifelse(
    china$tool_specified == 1L & china$linked_to_pass == 1L,
    "Full stack",
    ifelse(
      china$tool_specified == 1L & china$linked_to_pass == 0L,
      "Framework + tool specified",
      ifelse(
        china$tool_specified == 0L & china$linked_to_pass == 1L,
        "Framework + linked-to-pass",
        "Framework only"
      )
    )
  )
)

depth_order <- c(
  "Framework only",
  "Framework + linked-to-pass",
  "Framework + tool specified",
  "Full stack"
)

depth_adoption <- china %>%
  filter(check_framework == 1L) %>%
  count(enforcement_depth, name = "n_success") %>%
  right_join(data.frame(enforcement_depth = depth_order, stringsAsFactors = FALSE), by = "enforcement_depth") %>%
  mutate(
    n_success = ifelse(is.na(n_success), 0L, n_success),
    n_total = n_adopters,
    enforcement_depth = factor(enforcement_depth, levels = depth_order)
  ) %>%
  rowwise() %>%
  mutate(
    proportion = n_success / n_total,
    ci_low = exact_ci(n_success, n_total)["low"],
    ci_high = exact_ci(n_success, n_total)["high"]
  ) %>%
  ungroup() %>%
  arrange(enforcement_depth) %>%
  mutate(
    proportion_pct = 100 * proportion,
    ci_low_pct = 100 * ci_low,
    ci_high_pct = 100 * ci_high,
    denominator_scope = "adopters only"
  )

panel_a_label <- sprintf("A. Adoption by institution type (N = %d)", n_total)
panel_b_label <- sprintf("B. Enforcement depth among adopters (N = %d)", n_adopters)

plot_type <- type_adoption %>%
  mutate(
    category_plot = case_when(
      Inst_type == "agricultural universities" ~ "Agricultural",
      Inst_type == "comprehensive universities" ~ "Comprehensive",
      Inst_type == "ethnic/minority university" ~ "Ethnic/minority",
      Inst_type == "finance and economics universities" ~ "Finance and economics",
      Inst_type == "forestry universities" ~ "Forestry",
      Inst_type == "language and culture universities" ~ "Language and culture",
      Inst_type == "medical universities" ~ "Medical",
      Inst_type == "normal (teacher education) universities" ~ "Normal (teacher ed.)",
      Inst_type == "science and engineering universities" ~ "Science and engineering",
      Inst_type == "traditional Chinese medicine universities" ~ "Traditional Chinese medicine",
      TRUE ~ Inst_type
    )
  ) %>%
  transmute(
    panel = panel_a_label,
    category = category_plot,
    style_group = case_when(
      category_plot %in% c(
        "Science and engineering",
        "Medical",
        "Traditional Chinese medicine",
        "Agricultural",
        "Forestry"
      ) ~ "Science/tech",
      category_plot == "Comprehensive" ~ "Comprehensive",
      TRUE ~ "Humanities/social sciences"
    ),
    label_text = sprintf("%d (%d)", n_adopters, n_total),
    proportion = proportion,
    ci_low = ci_low,
    ci_high = ci_high
  )

plot_depth <- depth_adoption %>%
  transmute(
    panel = panel_b_label,
    category = enforcement_depth,
    style_group = "Enforcement depth",
    label_text = sprintf("%d (%d)", n_success, n_total),
    proportion = proportion,
    ci_low = ci_low,
    ci_high = ci_high
  )

plot_df <- bind_rows(plot_type, plot_depth)
plot_df$panel <- factor(plot_df$panel, levels = c(panel_a_label, panel_b_label))
plot_df$category <- factor(
  plot_df$category,
  levels = c(plot_type$category, depth_order)
)
plot_df$style_group <- factor(
  plot_df$style_group,
  levels = c(
    "Science/tech",
    "Comprehensive",
    "Humanities/social sciences",
    "Enforcement depth"
  )
)

FONT_FAMILY <- "sans"
FONT_SIZE_PT <- 14
FONT_SIZE_GEOM <- FONT_SIZE_PT / .pt
LINE_SIZE <- 0.25
TICK_LENGTH_PT <- 5

plot_palette <- c(
  "Science/tech" = "#0072B2",
  "Comprehensive" = "#7E3F98",
  "Humanities/social sciences" = "#D55E00",
  "Enforcement depth" = "#4D4D4D"
)

fill_palette <- c(
  "Science/tech" = "#D6E9F8",
  "Comprehensive" = "#EBDCF5",
  "Humanities/social sciences" = "#FDE1CF",
  "Enforcement depth" = "#E5E5E5"
)

shape_palette <- c(
  "Science/tech" = 21,
  "Comprehensive" = 22,
  "Humanities/social sciences" = 23,
  "Enforcement depth" = 21
)

legend_groups <- c(
  "Science/tech",
  "Comprehensive",
  "Humanities/social sciences"
)

p <- ggplot(plot_df, aes(x = category, y = proportion, color = style_group, fill = style_group, shape = style_group)) +
  geom_point(size = 4.2, stroke = 0.7, show.legend = TRUE) +
  geom_text(
    data = plot_df,
    aes(x = category, y = proportion, label = label_text),
    inherit.aes = FALSE,
    nudge_y = 0.12,
    size = FONT_SIZE_GEOM,
    family = FONT_FAMILY,
    color = "black",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = plot_palette,
    breaks = legend_groups,
    name = NULL
  ) +
  scale_fill_manual(values = fill_palette, breaks = legend_groups, guide = "none") +
  scale_shape_manual(values = shape_palette, breaks = legend_groups, guide = "none") +
  facet_wrap(~panel, ncol = 1, scales = "free_x") +
  scale_x_discrete(expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = scales::label_percent(accuracy = 1),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "China AI detection adoption proportions"
  ) +
  theme_classic(base_size = FONT_SIZE_PT) +
  theme(
    text = element_text(size = FONT_SIZE_PT, family = FONT_FAMILY),
    axis.text = element_text(size = FONT_SIZE_PT, color = "black"),
    axis.text.x = element_text(angle = 20, hjust = 1, vjust = 1),
    axis.title = element_text(size = FONT_SIZE_PT, color = "black"),
    axis.line = element_line(color = "black", linewidth = LINE_SIZE),
    axis.ticks = element_line(color = "black", linewidth = LINE_SIZE),
    axis.ticks.length = grid::unit(TICK_LENGTH_PT, "pt"),
    plot.title = element_text(size = FONT_SIZE_PT, face = "bold", hjust = 0.5),
    strip.text = element_text(size = FONT_SIZE_PT, face = "plain", color = "black"),
    strip.background = element_blank(),
    legend.position = c(0.69, 0.93),
    legend.justification = c(0, 1),
    legend.title = element_blank(),
    legend.text = element_text(size = FONT_SIZE_PT),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.margin = margin(10, 12, 10, 84)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = shape_palette[legend_groups],
        size = 3.8,
        stroke = 0.7,
        fill = fill_palette[legend_groups]
      )
    )
  ) +
  lemon::coord_capped_cart(
    bottom = lemon::capped_horizontal(),
    left = lemon::capped_vertical(capped = "both")
  )

plot_pdf <- file.path(output_dir, "F1_effectsizes_dotwhisker.pdf")
plot_svg <- file.path(output_dir, "F1_effectsizes_dotwhisker.svg")
plot_png <- file.path(output_dir, "F1_effectsizes_dotwhisker.png")

ggsave(plot_pdf, plot = p, width = 11.5, height = 8, units = "in", device = "pdf")
ggsave(plot_svg, plot = p, width = 11.5, height = 8, units = "in")
ggsave(plot_png, plot = p, width = 11.5, height = 8, units = "in", dpi = 300)

write.csv(headline_rate_cis, file.path(output_dir, "F1_china_headline_rate_cis.csv"), row.names = FALSE)
write.csv(type_effect_sizes, file.path(output_dir, "F1_china_type_effect_sizes.csv"), row.names = FALSE)
write.csv(type_adoption, file.path(output_dir, "F1_china_type_adoption_probabilities.csv"), row.names = FALSE)
write.csv(depth_adoption, file.path(output_dir, "F1_china_enforcement_depth_probabilities.csv"), row.names = FALSE)

cat("F1 effect-size extension complete. Outputs:\n")
cat(" - ", file.path(output_dir, "F1_china_headline_rate_cis.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F1_china_type_effect_sizes.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F1_china_type_adoption_probabilities.csv"), "\n", sep = "")
cat(" - ", file.path(output_dir, "F1_china_enforcement_depth_probabilities.csv"), "\n", sep = "")
cat(" - ", plot_pdf, "\n", sep = "")
cat(" - ", plot_svg, "\n", sep = "")
cat(" - ", plot_png, "\n", sep = "")
