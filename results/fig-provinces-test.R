# fig-provinces-test.R
# Supp Figure provinces-test x3

library(ggplot2)

province_pairs_Gini <- readRDS(here::here("results", "provinces_test_Gini.rds"))
province_pairs_LI <- readRDS(here::here("results", "provinces_test_LI.rds"))
province_pairs_VH <- readRDS(here::here("results", "provinces_test_VH.rds"))
province_pairs_CU <- readRDS(here::here("results", "provinces_test_CU.rds"))

# --- Unified colour scale (matches fig-combined-tests.R) ---

gap_colours <- c(
  "Close (1 year or <10% of range)"      = "#082511",
  "Near (2 years or 10-25% of range)"    = "#1b7837",
  "Moderate (3 years or 25-50% of range)" = "#5aae61",
  "Distant (>3 years or >50% of range)"  = "#a6dba0"
)

# Recode province gap_group labels to unified labels
prov_recode <- c("Similar (within 10% of range)"       = "Close (1 year or <10% of range)",
                 "Somewhat similar (10-25% of range)"  = "Near (2 years or 10-25% of range)",
                 "Somewhat distant (25-50% of range)"  = "Moderate (3 years or 25-50% of range)",
                 "Distant (>50% apart)"                = "Distant (>3 years or >50% of range)")

# enforce ordering of gap_group factor levels from scale above
province_pairs_Gini$gap_group <- factor(prov_recode[province_pairs_Gini$gap_group], levels = names(gap_colours))
province_pairs_LI$gap_group   <- factor(prov_recode[province_pairs_LI$gap_group],   levels = names(gap_colours))
province_pairs_VH$gap_group   <- factor(prov_recode[province_pairs_VH$gap_group],   levels = names(gap_colours))
province_pairs_CU$gap_group   <- factor(prov_recode[province_pairs_CU$gap_group],   levels = names(gap_colours))

# Bin settings (match fig-combined-tests.R)
bw <- 0.02

make_pt_test_fig <- function(data, legend_name) {
  # ...existing code...
  ggplot(data, aes(x = diff, fill = gap_group)) +
    geom_histogram(binwidth = bw, boundary = 0) +
    scale_fill_manual(values = gap_colours, name = stringr::str_wrap(legend_name, width = 20)) +
    facet_grid(rows = vars(gap_group), cols = vars(age_current),
               labeller = labeller(age_current = \(x) paste("Age", x)), scales = "free") +
    scale_x_continuous(breaks = seq(0, (ceiling(max(data$diff, na.rm = TRUE) / bw) + 1) * bw, by = 2*bw),
                       labels = scales::label_percent(suffix = "")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = scales::breaks_extended(n = 3)) +
    labs(x = "Absolute difference in vaccine coverage (%)", y = "Number of PT pairs") +
    theme(
      legend.position = "bottom",
      strip.text.y = element_blank(),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 10)
    ) +
    guides(fill = guide_legend(nrow = 2))
}

fig_pt_test2 <- make_pt_test_fig(province_pairs_LI |> dplyr::filter(age_current %in% c(7, 11, 15 ,21)), "PT similarity by children in low-income families")
fig_pt_test3 <- make_pt_test_fig(province_pairs_VH |> dplyr::filter(age_current %in% c(7, 11, 15 ,21)), "PT similarity by vaccine hesitancy")
fig_pt_test4 <- make_pt_test_fig(province_pairs_CU |> dplyr::filter(age_current %in% c(7, 11, 15 ,21)), "PT similarity by COVID unvaccinated")

ggsave(here::here("results", "fig-pt-test2.jpeg"), fig_pt_test2, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-pt-test3.jpeg"), fig_pt_test3, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-pt-test4.jpeg"), fig_pt_test4, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-pt-test2.pdf"),  fig_pt_test2, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test3.pdf"),  fig_pt_test3, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-pt-test4.pdf"),  fig_pt_test4, width = fig_w, height = fig_h)