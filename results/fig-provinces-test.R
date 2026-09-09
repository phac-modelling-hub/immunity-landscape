# fig-provinces-test.R
# Supp Figure provinces-test x3

province_pairs_Gini <- readRDS(here::here("results", "provinces_test_Gini.rds"))
province_pairs_LI <- readRDS(here::here("results", "provinces_test_LI.rds"))
province_pairs_VH <- readRDS(here::here("results", "provinces_test_VH.rds"))
province_pairs_CU <- readRDS(here::here("results", "provinces_test_CU.rds"))

gap_colours <- c("Similar (within 10% of range)"   = "#1b7837",
                 "Somewhat similar (10-25% of range)"  = "#5aae61",
                 "Somewhat distant (25-50% of range)"  = "#a6dba0",
                 "Distant (>50% apart)" = "#d9f0d3")

# enforce ordering of gap_group factor levels from scale above
province_pairs_Gini$gap_group <- factor(province_pairs_Gini$gap_group, levels = names(gap_colours))
province_pairs_LI$gap_group <- factor(province_pairs_LI$gap_group, levels = names(gap_colours))
province_pairs_VH$gap_group <- factor(province_pairs_VH$gap_group, levels = names(gap_colours))
province_pairs_CU$gap_group <- factor(province_pairs_CU$gap_group, levels = names(gap_colours))

fig_pt_test1 <- ggplot(province_pairs_Gini, aes(x = diff * 100, fill = gap_group)) +
  geom_histogram(binwidth = 2, boundary = 0, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = gap_colours, name = "PT similarity by Gini index") +
  facet_wrap(~ age_current) +
  labs(x = "Absolute difference in vaccine coverage (%)", y = "Number of PT pairs")
fig_pt_test2 <- ggplot(province_pairs_LI, aes(x = diff * 100, fill = gap_group)) +
  geom_histogram(binwidth = 2, boundary = 0, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = gap_colours, name = "PT similarity by children in low-income families") +
  facet_wrap(~ age_current) +
  labs(x = "Absolute difference in vaccine coverage (%)", y = "Number of PT pairs")
fig_pt_test3 <- ggplot(province_pairs_VH, aes(x = diff * 100, fill = gap_group)) +
  geom_histogram(binwidth = 2, boundary = 0, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = gap_colours, name = "PT similarity by vaccine hesitancy") +
  facet_wrap(~ age_current) +
  labs(x = "Absolute difference in vaccine coverage (%)", y = "Number of PT pairs")
fig_pt_test4 <- ggplot(province_pairs_CU, aes(x = diff * 100, fill = gap_group)) +
  geom_histogram(binwidth = 2, boundary = 0, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = gap_colours, name = "PT similarity by COVID unvaccinated") +
  facet_wrap(~ age_current) +
  labs(x = "Absolute difference in vaccine coverage (%)", y = "Number of PT pairs")

ggsave(here::here("results", "fig-pt-test1.pdf"),  fig_pt_test1, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test1.jpeg"), fig_pt_test1, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-pt-test2.pdf"),  fig_pt_test2, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test2.jpeg"), fig_pt_test2, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-pt-test3.pdf"),  fig_pt_test3, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test3.jpeg"), fig_pt_test3, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-pt-test4.pdf"),  fig_pt_test4, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test4.jpeg"), fig_pt_test4, width = fig_w, height = fig_h, dpi = 300)