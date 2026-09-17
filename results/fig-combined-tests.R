# fig-combined-tests.R
# Combined multipanel figure from fig-ages-test.R and fig-provinces-test.R

library(ggplot2)
library(patchwork)

# --- Unified colour scale ---

gap_colours <- c("Close (1 year or <10% of range)"       = "#D7191C",  # vivid red
                 "Near (2 years or 10-25% of range)"     = "#E66101",  # vivid orange
                 "Moderate (3 years or 25-50% of range)" = "#D9B000",  # deep gold (readable yellow)
                 "Distant (>3 years or >50% of range)"   = "#0072B2")  # vivid blue

# --- Ages test data and figures ---

age_pairs_1plus <- readRDS(here::here("results", "age_test_pairs.rds"))

# Recode gap_group to unified labels
age_recode <- c("1 year apart"   = "Close (1 year or <10% of range)",
                "2 years apart"  = "Near (2 years or 10-25% of range)",
                "3 years apart"  = "Moderate (3 years or 25-50% of range)",
                ">3 years apart" = "Distant (>3 years or >50% of range)")

age_pairs_1plus$gap_group <- factor(age_recode[age_pairs_1plus$gap_group], levels = names(gap_colours))

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

bands_ages <- make_bin_bands(age_pairs_1plus$diff, bw)

fig_ages_test_all <- ggplot(age_pairs_1plus, aes(x = diff, fill = gap_group)) +
  geom_rect(data = bands_ages, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey70", alpha = 0.5) +
  geom_histogram(binwidth = bw, center = 0, position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = gap_colours, name = stringr::str_wrap("Gap between pairs", width = 12)) +
  facet_wrap(~ location, ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, ceiling(max(age_pairs_1plus$diff, na.rm = TRUE) / bw) * bw, by = bw),
                     labels = scales::label_percent(suffix = "")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + 
  labs(
    title = "A: Among pairs of ages by PT",
    x = "Absolute difference in vaccine coverage (%)", y = "Number of age pairs") +
  theme(
    plot.title = element_text(size = 14)
  )

# --- Provinces test data and figures ---

province_pairs_Gini <- readRDS(here::here("results", "provinces_test_Gini.rds"))

# Recode gap_group to unified labels
prov_recode <- c("Similar (within 10% of range)"       = "Close (1 year or <10% of range)",
                 "Somewhat similar (10-25% of range)"  = "Near (2 years or 10-25% of range)",
                 "Somewhat distant (25-50% of range)"  = "Moderate (3 years or 25-50% of range)",
                 "Distant (>50% apart)"                = "Distant (>3 years or >50% of range)")

province_pairs_Gini$gap_group <- factor(prov_recode[province_pairs_Gini$gap_group], levels = names(gap_colours))

bands_pt <- make_bin_bands(province_pairs_Gini$diff, bw)

fig_pt_test1 <- ggplot(province_pairs_Gini, aes(x = diff, fill = gap_group)) +
  geom_rect(data = bands_pt, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
            fill = "grey70", alpha = 0.5) +
  geom_histogram(binwidth = bw, center = 0, position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = gap_colours, name = stringr::str_wrap("Gap between pairs", width = 12)) +
  facet_wrap(~ age_current, labeller = labeller(age_current = \(x) paste("Age", x)), ncol = 3, scales = "free_y") +
  scale_x_continuous(breaks = seq(0, ceiling(max(province_pairs_Gini$diff, na.rm = TRUE) / bw) * bw, by = bw),
                     labels = scales::label_percent(suffix = "")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "B: Among pairs of PTs by age",
    x = "Absolute difference in vaccine coverage (%)", y = "Number of PT pairs") +
  theme(plot.title = element_text(size = 14))

# --- Combine into multipanel figure ---
fig_combined_tests <- fig_ages_test_all / fig_pt_test1 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Similarity in vaccine coverage",
    tag_levels = NULL
  ) &
  theme(legend.position = "bottom", axis.text.y = element_text(size = 7), legend.title = element_text(size = 10), legend.text = element_text(size = 10)) &
  guides(fill = guide_legend(nrow = 2))
fig_combined_tests
# Save combined figure
ggsave(
  here::here("results", "fig-combined-tests.jpeg"),
  fig_combined_tests,
  width = fig_w,
  height = 2*fig_h,
  dpi = 300
)

ggsave(
  here::here("results", "fig-combined-tests.pdf"),
  fig_combined_tests,
  width = fig_w,
  height = 2*fig_h,
  dpi = 300
)
