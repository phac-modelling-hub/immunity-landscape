# fig-combined-tests.R
# Combined multipanel figure from fig-ages-test.R and fig-provinces-test.R

library(ggplot2)
library(patchwork)

# --- Unified colour scale ---

gap_colours <- c("Close (1 year or <10% of range)"      = "#1b7837",
                 "Near (2 years or 10-25% of range)"    = "#5aae61",
                 "Moderate (3 years or 25-50% of range)" = "#a6dba0",
                 "Distant (>3 years or >50% of range)"  = "#d9f0d3")

# --- Ages test data and figures ---

age_pairs_1plus <- readRDS(here::here("results", "ages_test_alldata.rds"))

# Recode gap_group to unified labels
age_recode <- c("1 year apart"   = "Close (1 year or <10% of range)",
                "2 years apart"  = "Near (2 years or 10-25% of range)",
                "3 years apart"  = "Moderate (3 years or 25-50% of range)",
                ">3 years apart" = "Distant (>3 years or >50% of range)")

age_pairs_1plus$gap_group <- factor(age_recode[age_pairs_1plus$gap_group], levels = names(gap_colours))

fig_ages_test_all <- ggplot(age_pairs_1plus, aes(x = diff * 100, fill = gap_group)) +
  geom_histogram(binwidth = 2, boundary = 0, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = gap_colours, name = stringr::str_wrap("Gap between pairs", width = 12)) +
  facet_wrap(~ location, labeller = labeller(location = \(x) stringr::str_wrap(x, width = 22)), ncol = 3) +
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

fig_pt_test1 <- ggplot(province_pairs_Gini, aes(x = diff * 100, fill = gap_group)) +
  geom_histogram(binwidth = 2, boundary = 0, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = gap_colours, name = stringr::str_wrap("Gap between pairs", width = 12)) +
  facet_wrap(~ age_current, labeller = labeller(age_current = \(x) paste("Age", x)), ncol = 3) +
  labs(
    title = "B: Among pairs of PTs by age",
    x = "Absolute difference in vaccine coverage (%)", y = "Number of PT pairs") +
  theme(plot.title = element_text(size = 14))

# --- Combine into multipanel figure ---
fig_combined_tests <- fig_ages_test_all + fig_pt_test1 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Similarity in vaccine coverage",
    tag_levels = NULL
  ) &
  theme(legend.position = "bottom") &
  guides(fill = guide_legend(nrow = 2))

# Save combined figure
ggsave(
  here::here("results", "fig-combined-tests.jpeg"),
  fig_combined_tests,
  width = 1.25*fig_w,
  height = 1.45*fig_h,
  dpi = 300
)

ggsave(
  here::here("results", "fig-combined-tests.pdf"),
  fig_combined_tests,
  width = 1.25*fig_w,
  height = 1.45*fig_h,
  dpi = 300
)
