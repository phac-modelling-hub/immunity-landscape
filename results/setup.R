# setup.R
# Load packages, source R functions, and define manuscript plot theme, palettes, and shapes.
# Source this file at the top of every figure/table script.

library(here)
library(ggrepel)
library(ggplot2)

# ── Load packages, functions, and cleaned coverage datasets ───────────────────
# Previously scraped from the initial_setup chunk of code-demo.qmd; now shared
# via results/load-data.R, which code-demo.qmd also sources.
source(here::here("results", "load-data.R"))

# ── Figure save dimensions ─────────────────────────────────────────────────────
fig_w <- 7.29   # inches
fig_h <- 4.51   # inches

# ── Manuscript ggplot theme ────────────────────────────────────────────────────
theme_ms <- theme_bw(base_size = 11) +
  theme(
    strip.text.x       = element_text(size = 8),
    axis.title       = element_text(size = 12),
    axis.text        = element_text(size = 10),
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 8),
    plot.title       = element_text(size = 16),
    panel.grid.minor = element_blank()
  )
theme_set(theme_ms)

# ── Colour palettes (from plot_coverage.R — viridis option G, colourblind-friendly) ──
scale_colour_ms <- function(...) scale_colour_viridis_d(option = "G", direction = -1, end = 0.9, ...)
scale_fill_ms   <- function(...) scale_fill_viridis_d(option = "G",   direction = -1, end = 0.9, ...)

# LOPO bar charts: full model preferred (blue) vs age-only model preferred (red)
# Usage: aes(fill = diff > 0) + scale_fill_manual(values = colours_lopo)
colours_lopo <- c("TRUE" = "#2166ac", "FALSE" = "#d73027")

# ── Shape mapping (from plot_coverage.R — n_doses categories) ─────────────────
# Assigned by factor level order of n_doses; see factorize() for level ordering.
shapes_doses <- c(18, 16, 17, 15, 13)  # diamond, circle, triangle, square, x-circle
