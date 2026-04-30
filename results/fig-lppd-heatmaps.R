# fig-lppd-heatmaps.R
# Supp Figure lppd-heatmaps: LPPD heatmaps over (l1, l2) for Canada models,
# one panel per model framework (Gini, LI, VH), faceted by meas_error.

make_heatmap <- function(csv_name, title_lab, k_filter = "kexp", b_filter = 0.5) {
  read_csv(here::here("results", csv_name), show_col_types = FALSE) %>%
    filter(k_name == k_filter, b == b_filter) %>%
    mutate(l1 = factor(l1), l2 = factor(l2),
           lppd_capped = if_else(lppd_exact < -100, NA_real_, lppd_exact)) %>%
    ggplot(aes(x = l1, y = l2, fill = lppd_capped)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#d73027", na.value = "white",
                        name = "LOO lppd") +
    facet_wrap(~ meas_error, labeller = label_both) +
    labs(title = paste0("LPPD heatmap — ", title_lab, " model"), x = "l1", y = "l2")
}

fig_heatmap_Gini <- make_heatmap("GPcombinations_Gini_witherror.csv",        "Gini")
fig_heatmap_LI   <- make_heatmap("GPcombinations_lowincome_witherror.csv",   "Low-income")
fig_heatmap_VH   <- make_heatmap("GPcombinations_vaccinehesitancy_witherror.csv", "Vaccine hesitancy")

ggsave(here::here("results", "fig-lppd-heatmaps_Gini.pdf"), fig_heatmap_Gini, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-lppd-heatmaps_LI.pdf"),   fig_heatmap_LI,   width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-lppd-heatmaps_VH.pdf"),   fig_heatmap_VH,   width = fig_w, height = fig_h)
