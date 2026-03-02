################################################################################
# Fig. 1: Thesis AI-detection enforcement stack (China) vs governance dimensions
# Zhicheng Lin
# Last updated: Feb 27, 2026
# Style note: aligned to DataPlot house style checklist (2026-02-15)
################################################################################

# ==============================================================================
# PACKAGES & ENVIRONMENT
# ==============================================================================

package_list <- c("ggplot2", "patchwork", "dplyr", "lemon", "svglite")

# Function to check, (optionally) install, and load packages
load_packages <- function(pkgs, install_missing = FALSE) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0 && install_missing) {
    install.packages(missing_pkgs)
  }
  invisible(lapply(pkgs, library, character.only = TRUE))
}

load_packages(package_list, install_missing = FALSE)

# Resolve script path so IO works in both RStudio and command-line runs.
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    return(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
  return(normalizePath("."))
}

script_dir <- get_script_dir()

# ==============================================================================
# GLOBAL PARAMETERS
# ==============================================================================

# House-style canonical constants
FONT_FAMILY    <- "sans"
FONT_SIZE_PT   <- 14
FONT_SIZE_GEOM <- FONT_SIZE_PT / .pt
LINE_SIZE      <- 0.25
TICK_LENGTH_PT <- 5

# --- Global palette (lifted out of individual scales; values unchanged) ---
COL_CHINA <- c(
  "Full"         = "#E67E22",
  "M+T"          = "#F39C12",
  "M+L"          = "#FAD7A0",
  "M_only"       = "#FDEBD0",
  "No mandatory" = "#F2F2F2"
)

COL_TABLE <- c(
  "Full stack"                = "#E67E22",
  "Mandate + tool specified"  = "#F39C12",
  "Mandate + linked-to-pass"  = "#FAD7A0",
  "Mandate only"              = "#FDEBD0"
)

COL_INTL_BAR <- "#4A81BF"

# Output dimensions (inches)
FIG_WIDTH  <- 11.5
FIG_HEIGHT <- 6.3

# ==============================================================================
# SHARED HELPERS & THEME
# ==============================================================================

# Conventional 1-decimal rounding (half-up), avoids R's default banker's rounding
round1_up <- function(x) floor(x * 10 + 0.5) / 10

# Minimal shared theme: sets global typography consistently without “re-enabling”
# elements removed by theme_void(), etc.
theme_fig1_shared <- function() {
  theme_classic(base_size = FONT_SIZE_PT) +
    theme(
      text              = element_text(size = FONT_SIZE_PT, family = FONT_FAMILY),
      axis.text         = element_text(size = FONT_SIZE_PT, color = "black"),
      axis.title        = element_text(size = FONT_SIZE_PT, color = "black"),
      axis.line         = element_line(color = "black", linewidth = LINE_SIZE),
      axis.ticks        = element_line(color = "black", linewidth = LINE_SIZE),
      axis.ticks.length = grid::unit(TICK_LENGTH_PT, "pt"),
      plot.title        = element_text(hjust = 0.5, size = FONT_SIZE_PT, face = "bold"),
      plot.tag          = element_text(size = FONT_SIZE_PT, face = "bold"),
      legend.position   = "none"
    )
}

# ==============================================================================
# DATA INPUTS (read from canonical CSVs)
# ==============================================================================

china_file <- normalizePath(file.path(script_dir, "../../../20_coding/23_results/S1_china_irr_base.csv"), mustWork = TRUE)
intl_file  <- normalizePath(file.path(script_dir, "../../../20_coding/21_store/S2_international_analysis.csv"), mustWork = TRUE)
output_dir <- normalizePath(file.path(script_dir, "../results"), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(china_file)) stop("Missing file: ", china_file)
if (!file.exists(intl_file))  stop("Missing file: ", intl_file)

china_raw <- read.csv(china_file, check.names = FALSE, stringsAsFactors = FALSE)
intl_raw  <- read.csv(intl_file,  check.names = FALSE, stringsAsFactors = FALSE)

# Defensive: force 0/1 columns to integer
china_code_cols <- c("check_framework(0/1)", "tool_specified(0/1)", "linked_to_pass(0/1)")
china_raw[china_code_cols] <- lapply(china_raw[china_code_cols], as.integer)

intl_code_cols <- c("D1_tech_limit(0/1)", "D2_proactive(0/1)", "D3_responsibility(0/1)", "D4_ai_as_aid(0/1)")
intl_raw[intl_code_cols] <- lapply(intl_raw[intl_code_cols], as.integer)

N_china <- nrow(china_raw)
N_intl  <- nrow(intl_raw)

# ==============================================================================
# PANEL A: DATA PREPARATION (China enforcement stack)
# ==============================================================================

china_levels <- c("Full", "M+T", "M+L", "M_only", "No mandatory")

china_df <- china_raw %>%
  mutate(Category = case_when(
    `check_framework(0/1)` == 0L ~ "No mandatory",
    `tool_specified(0/1)` == 1L & `linked_to_pass(0/1)` == 1L ~ "Full",
    `tool_specified(0/1)` == 1L & `linked_to_pass(0/1)` == 0L ~ "M+T",
    `tool_specified(0/1)` == 0L & `linked_to_pass(0/1)` == 1L ~ "M+L",
    TRUE ~ "M_only"
  )) %>%
  count(Category, name = "Count") %>%
  right_join(data.frame(Category = china_levels), by = "Category") %>%
  mutate(
    Count = ifelse(is.na(Count), 0L, Count),
    Category = factor(Category, levels = china_levels),
    pct = round1_up(Count / N_china * 100),
    Label = case_when(
      Category == "Full" ~ sprintf("Full stack: mandate + tool + linked: %d (%.1f%%)", Count, pct),
      Category == "M+T" ~ sprintf("Mandate + tool specified: %d (%.1f%%)", Count, pct),
      Category == "M+L" ~ sprintf("Mandate + linked-to-pass: %d (%.1f%%)", Count, pct),
      Category == "M_only" ~ sprintf("Mandate only: %d (%.1f%%)", Count, pct),
      Category == "No mandatory" ~ sprintf("No mandatory detection: %d (%.1f%%)", Count, pct)
    )
  ) %>%
  arrange(Category)

mandated_n     <- sum(china_raw$`check_framework(0/1)` == 1L)
no_mandatory_n <- sum(china_raw$`check_framework(0/1)` == 0L)

mandated_label <- sprintf(
  "Mandated\ndetection:\n%d (%.1f%%)",
  mandated_n, round1_up(mandated_n / N_china * 100)
)

# Connector y positions aligned to the stacked segments
china_y <- china_df %>%
  mutate(
    cat_chr = as.character(Category),
    # ggplot stacking order matches factor levels from TOP->BOTTOM in this figure;
    # we compute midpoints from bottom->top using rev(levels)
    stack_order = factor(cat_chr, levels = rev(china_levels))
  ) %>%
  arrange(stack_order) %>%
  mutate(
    ymax = cumsum(Count),
    ymin = lag(ymax, default = 0),
    ymid = (ymin + ymax) / 2
  )

y_full  <- china_y$ymid[china_y$cat_chr == "Full"]

# Small nudge to separate top label from cap while preserving segment mapping.
full_stack_nudge <- 3
y_full_callout <- pmin(N_china - 0.5, y_full + full_stack_nudge)
y_mt    <- china_y$ymid[china_y$cat_chr == "M+T"]
y_ml    <- china_y$ymid[china_y$cat_chr == "M+L"]
y_monly <- china_y$ymid[china_y$cat_chr == "M_only"]
y_no    <- max(72, china_y$ymid[china_y$cat_chr == "No mandatory"])

# ==============================================================================
# PANEL A: INSET TABLE (computed from latest China file)
# ==============================================================================

table_df <- china_raw %>%
  filter(`check_framework(0/1)` == 1L) %>%
  transmute(
    Linked = ifelse(`linked_to_pass(0/1)` == 1L, "Linked-to-pass", "Not linked-to-pass"),
    Tool   = ifelse(`tool_specified(0/1)` == 1L, "Tool\nspecified", "Tool not\nspecified")
  ) %>%
  count(Linked, Tool, name = "Value")

# Ensure all 2×2 combos exist even if some are 0
all_combos <- expand.grid(
  Linked = c("Linked-to-pass", "Not linked-to-pass"),
  Tool   = c("Tool\nspecified", "Tool not\nspecified"),
  stringsAsFactors = FALSE
)

table_df <- all_combos %>%
  left_join(table_df, by = c("Linked", "Tool")) %>%
  mutate(
    Value = ifelse(is.na(Value), 0L, Value),
    Cell = case_when(
      Linked == "Linked-to-pass"     & Tool == "Tool\nspecified"     ~ "Full stack",
      Linked == "Not linked-to-pass" & Tool == "Tool\nspecified"     ~ "Mandate + tool specified",
      Linked == "Linked-to-pass"     & Tool == "Tool not\nspecified" ~ "Mandate + linked-to-pass",
      TRUE                                                       ~ "Mandate only"
    ),
    Linked = factor(Linked, levels = c("Not linked-to-pass", "Linked-to-pass")),
    Tool   = factor(Tool, levels = c("Tool\nspecified", "Tool not\nspecified")),
    Cell   = factor(Cell, levels = c("Full stack", "Mandate + tool specified", "Mandate + linked-to-pass", "Mandate only"))
  )

table_core <- ggplot(table_df, aes(x = Tool, y = Linked, label = Value)) +
  geom_tile(aes(fill = Cell), color = "black", linewidth = LINE_SIZE) +
  geom_text(size = FONT_SIZE_GEOM, family = FONT_FAMILY) +
  scale_fill_manual(values = COL_TABLE) +
  labs(title = "Within mandatory group") +
  scale_x_discrete(expand = c(0,0), position = "top") +
  scale_y_discrete(expand = c(0,0)) +
  theme_fig1_shared() +
  theme(
    plot.title = element_text(hjust = 0.5, size = FONT_SIZE_PT, face = "plain", margin = margin(b = 4)),
    axis.title = element_blank(),
    axis.text.x = element_text(size = FONT_SIZE_PT, color = "black", margin = margin(b = 2)),
    axis.text.y = element_text(size = FONT_SIZE_PT, color = "black", hjust = 1, margin = margin(r = 6)),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.background = element_rect(color = "black", linewidth = LINE_SIZE, fill = "white"),
    plot.margin = margin(10, 12, 10, 12)
  )

# ==============================================================================
# PANEL A: PLOT CONSTRUCTION
# ==============================================================================

extraX <- 0.45

p_a <- ggplot(china_df, aes(x = 1, y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 0.35, color = "white", linewidth = LINE_SIZE) +
  scale_fill_manual(values = COL_CHINA) +
  
  # --- Left Side Arrow ---
  annotate("segment", x = 0.65, xend = 0.65, y = 0, yend = N_china,
           arrow = grid::arrow(length = grid::unit(0.2, "cm"), type = "closed"),
           linewidth = LINE_SIZE) +
  annotate("text", x = 0.34, y = N_china/2, label = "More levers stacked",
           angle = 90, size = FONT_SIZE_GEOM, family = FONT_FAMILY, fontface = "plain", hjust = 0.5) +
  
  # --- Right Side Connectors (segment-mapped) ---
  annotate("segment", x = 1.18, xend = 1.50, y = y_full_callout, yend = y_full_callout, linewidth = LINE_SIZE) +
  annotate("segment", x = 1.18, xend = 1.50, y = y_mt, yend = y_mt, linewidth = LINE_SIZE) +
  annotate("segment", x = 1.18, xend = 1.50, y = y_ml, yend = y_ml, linewidth = LINE_SIZE) +
  annotate("segment", x = 1.18, xend = 1.50, y = y_monly, yend = y_monly, linewidth = LINE_SIZE) +
  annotate("segment", x = 1.18, xend = 1.50, y = y_no, yend = y_no, linewidth = LINE_SIZE) +

  # Labels
  annotate("text", x = 1.52, y = y_full_callout, label = china_df$Label[1], hjust = 0, size = FONT_SIZE_GEOM, family = FONT_FAMILY, lineheight = 1) +
  annotate("text", x = 1.52, y = y_mt, label = china_df$Label[2], hjust = 0, size = FONT_SIZE_GEOM, family = FONT_FAMILY) +
  annotate("text", x = 1.52, y = y_ml, label = china_df$Label[3], hjust = 0, size = FONT_SIZE_GEOM, family = FONT_FAMILY) +
  annotate("text", x = 1.52, y = y_monly, label = china_df$Label[4], hjust = 0, size = FONT_SIZE_GEOM, family = FONT_FAMILY) +
  annotate("text", x = 1.52, y = y_no, label = china_df$Label[5], hjust = 0, size = FONT_SIZE_GEOM, family = FONT_FAMILY) +
  
  # --- "Mandated Detection" Bracket (UPDATED) ---
  annotate("segment", x = 8.35, xend = 8.40, y = no_mandatory_n, yend = no_mandatory_n, linewidth = LINE_SIZE) +
  annotate("segment", x = 8.35, xend = 8.40, y = N_china, yend = N_china, linewidth = LINE_SIZE) +
  annotate("segment", x = 8.40, xend = 8.40, y = no_mandatory_n, yend = N_china, linewidth = LINE_SIZE) +
  annotate("text", x = 8.60, y = (no_mandatory_n + N_china)/2, label = mandated_label,
           hjust = 0, size = FONT_SIZE_GEOM, family = FONT_FAMILY, lineheight = 1) +
  
  scale_x_continuous(limits = c(0.30, 9.20), breaks = NULL, labels = NULL) +
  scale_y_continuous(limits = c(0, 135), breaks = seq(0, 125, 25), labels = seq(0, 125, 25), expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  theme_fig1_shared() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.title = element_text(face = "bold", size = FONT_SIZE_PT, hjust = 0.5, lineheight = 1.25, margin = margin(b = 2)),
    plot.margin = margin(t = 8, r = 18, b = 8, l = 8, unit = "pt")
  ) +
  labs(title = sprintf("China (N = %d)\nThesis AI-detection enforcement stack", N_china))

# ==============================================================================
# PANEL B: DATA PREPARATION + PLOT (International governance dimensions)
# ==============================================================================

intl_df <- data.frame(
  Dim = factor(
    c("D1\nReliability",
      "D2\nDisclosure",
      "D3\nResponsibility",
      "D4\nAI as aid"),
    levels = c("D1\nReliability",
               "D2\nDisclosure",
               "D3\nResponsibility",
               "D4\nAI as aid")
  ),
  Count = c(
    sum(intl_raw$`D1_tech_limit(0/1)` == 1L, na.rm = TRUE),
    sum(intl_raw$`D2_proactive(0/1)` == 1L, na.rm = TRUE),
    sum(intl_raw$`D3_responsibility(0/1)` == 1L, na.rm = TRUE),
    sum(intl_raw$`D4_ai_as_aid(0/1)` == 1L, na.rm = TRUE)
  )
)

p_b <- ggplot(intl_df, aes(x = Dim, y = Count/N_intl*100)) +
  geom_bar(stat = "identity", fill = COL_INTL_BAR, color = "black", linewidth = LINE_SIZE, width = 0.4) +
  geom_text(aes(label = paste0(round1_up(Count/N_intl*100), "%\n(", Count, ")")),
            vjust = -0.3, size = FONT_SIZE_GEOM, family = FONT_FAMILY, fontface = "plain", lineheight = 1) +
  scale_y_continuous(
    limits = c(0, 115),
    breaks = seq(0, 100, 25),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  scale_x_discrete(drop = FALSE) +
  coord_capped_cart(
    bottom = capped_horizontal(),
    left   = capped_vertical(capped = "both")
  ) +
  theme_fig1_shared() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(color = "black", size = FONT_SIZE_PT, margin = margin(t = 2), lineheight = 0.95),
    axis.text.y = element_text(color = "black", size = FONT_SIZE_PT),
    plot.title = element_text(face = "bold", size = FONT_SIZE_PT, hjust = 0.5, lineheight = 1.25, margin = margin(b = 2)),
    plot.margin = margin(t = 8, r = 8, b = 8, l = 18, unit = "pt")
  ) +
  labs(title = sprintf("International (N = %d)\nGovernance dimensions", N_intl))

# ==============================================================================
# ASSEMBLY & EXPORT
# ==============================================================================

p_a_final <- p_a +
  inset_element(
    table_core,
    left = 0.26, bottom = 0.00, right = 0.99, top = 0.40,
    align_to = "panel"
  )

# Manual a./b. tags are set in p_a/p_b because auto-tagging also tags the inset.
final_plot <- (p_a_final + p_b + plot_layout(widths = c(1.65, 1.35)))

save_device <- tryCatch({
  cairo_pdf(tempfile())
  dev.off()
  cairo_pdf
}, error = function(e) "pdf", warning = function(w) "pdf")

ggsave(
  filename = file.path(output_dir, "fig1new.pdf"),
  plot     = final_plot,
  width    = FIG_WIDTH,
  height   = FIG_HEIGHT,
  units    = "in",
  device   = save_device
)

ggsave(
  filename = file.path(output_dir, "fig1new.svg"),
  plot     = final_plot,
  device   = svglite::svglite,
  width    = FIG_WIDTH,
  height   = FIG_HEIGHT,
  units    = "in"
)

ggsave(
  filename = file.path(output_dir, "fig1new.png"),
  plot     = final_plot,
  width    = FIG_WIDTH,
  height   = FIG_HEIGHT,
  units    = "in",
  dpi      = 300
)
