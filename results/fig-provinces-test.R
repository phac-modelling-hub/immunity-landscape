# fig-provinces-test.R
# Supp Figure provinces-test x3

library(ggplot2)

province_pairs_Gini <- readRDS(here::here("results", "provinces_test_Gini.rds"))
province_pairs_LI <- readRDS(here::here("results", "provinces_test_LI.rds"))
province_pairs_VH <- readRDS(here::here("results", "provinces_test_VH.rds"))
province_pairs_CU <- readRDS(here::here("results", "provinces_test_CU.rds"))

# --- Unified colour scale (matches fig-combined-tests.R) ---

gap_colours <- c("Similar (within 10% of range)"      = "#D7191C",  # vivid red
                 "Somewhat similar (10-25% of range)" = "#E66101",  # vivid orange
                 "Somewhat distant (25-50% of range)" = "#D9B000",  # deep gold (readable yellow)
                 "Distant (>50% apart)"               = "#0072B2")  # vivid blue

# enforce ordering of gap_group factor levels from scale above
province_pairs_Gini$gap_group <- factor(province_pairs_Gini$gap_group, levels = names(gap_colours))
province_pairs_LI$gap_group <- factor(province_pairs_LI$gap_group, levels = names(gap_colours))
province_pairs_VH$gap_group <- factor(province_pairs_VH$gap_group, levels = names(gap_colours))
province_pairs_CU$gap_group <- factor(province_pairs_CU$gap_group, levels = names(gap_colours))

# Bin settings and helper for centred bins + banding
bw <- 0.05

make_bin_bands <- function(x, binwidth) {
  max_bin <- ceiling(max(x, na.rm = TRUE) / binwidth)
  k <- 0:max_bin
  data.frame(
    xmin = k * binwidth - binwidth / 2,
    xmax = k * binwidth + binwidth / 2,
    shade = k %% 2 == 0
  ) |>
    dplyr::filter(shade)
}

make_pt_test_fig <- function(data, legend_name) {
  bands <- make_bin_bands(data$diff, bw)
  ggplot(data, aes(x = diff, fill = gap_group)) +
    geom_rect(data = bands, inherit.aes = FALSE,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              fill = "grey70", alpha = 0.5) +
    geom_histogram(binwidth = bw, center = 0, position = position_dodge2(preserve = "single")) +
    scale_fill_manual(values = gap_colours, name = stringr::str_wrap(legend_name, width = 20)) +
    facet_wrap(~ age_current, labeller = labeller(age_current = \(x) paste("Age", x)), scales = "free_y") +
    scale_x_continuous(breaks = seq(0, ceiling(max(data$diff, na.rm = TRUE) / bw) * bw, by = bw),
                       labels = scales::label_percent(suffix = "")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "Absolute difference in vaccine coverage (%)", y = "Number of PT pairs") +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 7),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    ) +
    guides(fill = guide_legend(nrow = 2))
}

fig_pt_test1 <- make_pt_test_fig(province_pairs_Gini, "PT similarity by Gini index")
fig_pt_test2 <- make_pt_test_fig(province_pairs_LI, "PT similarity by children in low-income families")
fig_pt_test3 <- make_pt_test_fig(province_pairs_VH, "PT similarity by vaccine hesitancy")
fig_pt_test4 <- make_pt_test_fig(province_pairs_CU, "PT similarity by COVID unvaccinated")

ggsave(here::here("results", "fig-pt-test1.pdf"),  fig_pt_test1, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test1.jpeg"), fig_pt_test1, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-pt-test2.pdf"),  fig_pt_test2, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test2.jpeg"), fig_pt_test2, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-pt-test3.pdf"),  fig_pt_test3, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test3.jpeg"), fig_pt_test3, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-pt-test4.pdf"),  fig_pt_test4, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test4.jpeg"), fig_pt_test4, width = fig_w, height = fig_h, dpi = 300)