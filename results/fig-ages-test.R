# fig-ages-test.R
# Supp ages-test: pairwise absolute coverage differences by age gap, by province.
# Produces one figures: 1+ dose (ages-test1).

province_pairs_1plus <- readRDS(here::here("results", "province_pairs_1plus.rds"))

gap_colours <- c("1 year apart"   = "#1b7837",
                 "2 years apart"  = "#5aae61",
                 "3 years apart"  = "#a6dba0",
                 ">3 years apart" = "#d9f0d3")

# enforce ordering of gap_group factor levels from scale above
province_pairs_1plus$gap_group <- factor(province_pairs_1plus$gap_group, levels = names(gap_colours))

fig_ages_test1 <- ggplot(province_pairs_1plus, aes(x = diff * 100, fill = gap_group)) +
  geom_histogram(binwidth = 2, boundary = 0, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = gap_colours, name = "Age gap") +
  facet_wrap(~ location) +
  labs(title = "Vaccine coverage differences between pairs of ages (1+ doses)",
       x = "Absolute difference in vaccine coverage (%)", y = "Number of age pairs")

ggsave(here::here("results", "fig-ages-test1.pdf"),  fig_ages_test1, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-ages-test1.jpeg"), fig_ages_test1, width = fig_w, height = fig_h, dpi = 300)