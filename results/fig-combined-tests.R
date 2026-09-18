# fig-combined-tests.R
# Combined multipanel figure from fig-ages-test.R and fig-provinces-test.R

library(ggplot2)
library(patchwork)

# --- Unified colour scale ---

gap_colours <- c(
  "Close (1 year or <10% of range)"      = "#082511",
  "Near (2 years or 10-25% of range)"    = "#1b7837",
  "Moderate (3 years or 25-50% of range)" = "#5aae61",
  "Distant (>3 years or >50% of range)"  = "#a6dba0"
)

# --- Ages test data and figures ---

age_pairs_1plus <- readRDS(here::here("results", "age_test_pairs.rds"))

# Recode gap_group to unified labels
age_recode <- c("1 year apart"   = "Close (1 year or <10% of range)",
                "2 years apart"  = "Near (2 years or 10-25% of range)",
                "3 years apart"  = "Moderate (3 years or 25-50% of range)",
                ">3 years apart" = "Distant (>3 years or >50% of range)")

age_pairs_1plus$gap_group <- factor(age_recode[age_pairs_1plus$gap_group], levels = names(gap_colours))

# Keep only locations with data across all four gap_group options
age_pairs_1plus <- age_pairs_1plus |>
  dplyr::group_by(location) |>
  dplyr::filter(dplyr::n_distinct(gap_group) == length(gap_colours)) |>
  dplyr::ungroup()

# Bin settings
bw <- 0.02

fig_ages_test_all <- ggplot(age_pairs_1plus, aes(x = diff, fill = gap_group)) +
  geom_histogram(binwidth = bw, boundary = 0) +
  scale_fill_manual(values = gap_colours, name = stringr::str_wrap("Gap between pairs", width = 12)) +
  facet_grid(rows = vars(gap_group), cols = vars(location), scales = "free") +
  scale_x_continuous(breaks = seq(0, (ceiling(max(age_pairs_1plus$diff, na.rm = TRUE) / bw) + 1) * bw, by = 2*bw),
                     labels = scales::label_percent(suffix = "")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::breaks_extended(n = 3)) + 
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

# Keep only ages with data across all four gap_group options
province_pairs_Gini <- province_pairs_Gini |>
  dplyr::group_by(age_current) |>
  dplyr::filter(dplyr::n_distinct(gap_group) == length(gap_colours)) |>
  dplyr::ungroup()

fig_pt_test1 <- ggplot(province_pairs_Gini, aes(x = diff, fill = gap_group)) +
  geom_histogram(binwidth = bw, boundary = 0) +
  scale_fill_manual(values = gap_colours, name = stringr::str_wrap("Gap between pairs", width = 12)) +
  facet_grid(rows = vars(gap_group), cols = vars(age_current), labeller = labeller(age_current = \(x) paste("Age", x)), scales = "free") +
  scale_x_continuous(breaks = seq(0, (ceiling(max(province_pairs_Gini$diff, na.rm = TRUE) / bw) + 1) * bw, by = 2*bw),
                     labels = scales::label_percent(suffix = "")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::breaks_extended(n = 3)) +
  labs(
    title = "B: Among pairs of PTs by age",
    x = "Absolute difference in vaccine coverage (%)", y = "Number of PT pairs") +
  theme(
    plot.title = element_text(size = 14)
  )

# --- Combine into multipanel figure ---
fig_combined_tests <- fig_ages_test_all / fig_pt_test1 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Similarity in vaccine coverage",
    tag_levels = NULL
  ) &
  theme(
    legend.position = "bottom", 
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 10),
    strip.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  ) &
  guides(fill = guide_legend(nrow = 2))

# Save combined figure
ggsave(
  here::here("results", "fig-combined-tests.jpeg"),
  fig_combined_tests,
  width = fig_w,
  height = 1.5*fig_h,
  dpi = 300
)

ggsave(
  here::here("results", "fig-combined-tests.pdf"),
  fig_combined_tests,
  width = 2*fig_w,
  height = fig_h,
  dpi = 300
)
