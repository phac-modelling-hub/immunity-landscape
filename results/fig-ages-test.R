# fig-ages-test.R
# Supp ages-test: pairwise absolute coverage differences by age gap, by province.
# Produces two figures: testing the whole model training dataset, and just data from provinces.

age_pairs_1plus <- readRDS(here::here("results", "ages_test_alldata.rds"))
age_pairs_PTsonly <- readRDS(here::here("results", "ages_test_PTdata.rds"))

gap_colours <- c("1 year apart"   = "#1b7837",
                 "2 years apart"  = "#5aae61",
                 "3 years apart"  = "#a6dba0",
                 ">3 years apart" = "#d9f0d3")

# enforce ordering of gap_group factor levels from scale above
age_pairs_1plus$gap_group <- factor(age_pairs_1plus$gap_group, levels = names(gap_colours))
age_pairs_PTsonly$gap_group <- factor(age_pairs_PTsonly$gap_group, levels = names(gap_colours))

fig_ages_test_all <- ggplot(age_pairs_1plus, aes(x = diff * 100, fill = gap_group)) +
  geom_histogram(binwidth = 2, boundary = 0, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = gap_colours, name = "Age gap") +
  facet_wrap(~ location) +
  labs(x = "Absolute difference in vaccine coverage (%)", y = "Number of age pairs")
fig_ages_test_PTs <- ggplot(age_pairs_PTsonly, aes(x = diff * 100, fill = gap_group)) +
  geom_histogram(binwidth = 2, boundary = 0, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = gap_colours, name = "Age gap") +
  facet_wrap(~ location) +
  labs(x = "Absolute difference in vaccine coverage (%)", y = "Number of age pairs")

ggsave(here::here("results", "fig_ages_test_all.pdf"),  fig_ages_test_all, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig_ages_test_all.jpeg"), fig_ages_test_all, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig_ages_test_PTs.pdf"),  fig_ages_test_PTs, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig_ages_test_PTs.jpeg"), fig_ages_test_PTs, width = fig_w, height = fig_h, dpi = 300)