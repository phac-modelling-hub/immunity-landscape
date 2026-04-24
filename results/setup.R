# setup.R
# Load packages, source R functions, and define manuscript plot theme, palettes, and shapes.
# Source this file at the top of every figure/table script.

# ── Helper: extract and run a named chunk from a .qmd file ────────────────────
source_qmd_chunk <- function(qmd_file, chunk_label) {
  lines       <- readLines(qmd_file)
  label_line  <- grep(paste0("#\\| label: ", chunk_label), lines)
  if (length(label_line) == 0) stop("Chunk '", chunk_label, "' not found in ", qmd_file)
  open_fence  <- max(grep("^```\\{r", lines[1:label_line]))
  close_fence <- label_line - 1 + min(grep("^```$", lines[label_line:length(lines)]))
  code_lines  <- lines[(open_fence + 1):(close_fence - 1)]
  code_lines  <- code_lines[!grepl("^#\\|", code_lines)]  # strip chunk options
  tmp <- tempfile(fileext = ".R")
  writeLines(code_lines, tmp)
  source(tmp, local = FALSE)  # local=FALSE runs in global env
}

# ── Run gp-model.qmd initial_setup chunk ──────────────────────────────────────
source_qmd_chunk(here::here("gp-model.qmd"), "initial_setup")

# ── Manuscript ggplot theme ────────────────────────────────────────────────────
theme_ms <- theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(size = 10, face = "bold"),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 10),
    legend.title     = element_text(size = 11),
    legend.text      = element_text(size = 10),
    plot.title       = element_text(size = 13, face = "bold"),
    panel.grid.minor = element_blank()
  )
theme_set(theme_ms)

# ── Colour palettes (from plot_coverage.R — viridis option G, colourblind-friendly) ──
scale_colour_ms <- function(...) scale_colour_viridis_d(option = "G", direction = -1, end = 0.9, ...)
scale_fill_ms   <- function(...) scale_fill_viridis_d(option = "G",   direction = -1, end = 0.9, ...)

# Two-category fill (e.g. LOPO bar charts: full model vs age-only model)
colours_binary <- c("TRUE" = "#2166ac", "FALSE" = "#d73027")

# ── Shape mapping (from plot_coverage.R — n_doses categories) ─────────────────
shapes_doses <- c(18, 16, 17, 15, 13)  # diamond, circle, triangle, square, x-circle
