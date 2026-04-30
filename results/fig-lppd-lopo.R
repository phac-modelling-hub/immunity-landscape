# fig-lppd-lopo.R
# Supp Figure lppd-lopo: LOPO bar charts comparing full model vs age-only model per province.

lopo_Gini <- readRDS(here::here("results", "lopo_Gini.rds"))
lopo_LI   <- readRDS(here::here("results", "lopo_LI.rds"))
lopo_VH   <- readRDS(here::here("results", "lopo_VH.rds"))

make_lopo_plot <- function(lopo_df, title_lab) {
  ggplot(lopo_df, aes(x = pt, y = diff, fill = diff > 0)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
    scale_fill_manual(
      values = colours_lopo,
      labels = c("TRUE" = "Full model preferred", "FALSE" = "Age model preferred"),
      name   = NULL
    ) +
    labs(x     = "Province",
         y     = "Difference in mean LPPD per province",
         title = paste0("Relative LOPO performance (", title_lab, " model)")) +
    ylim(-0.75, 0.75)
}

fig_lopo_Gini <- make_lopo_plot(lopo_Gini, "Gini")
fig_lopo_LI   <- make_lopo_plot(lopo_LI,   "Low-income")
fig_lopo_VH   <- make_lopo_plot(lopo_VH,   "Vaccine hesitancy")

fig_lopo_combined <- fig_lopo_Gini / fig_lopo_LI / fig_lopo_VH

ggsave(here::here("results", "fig-lppd-lopo.pdf"),
       fig_lopo_combined, width = fig_w, height = fig_h * 2.2)
ggsave(here::here("results", "fig-lppd-lopo.jpeg"),
       fig_lopo_combined, width = fig_w, height = fig_h * 2.2, dpi = 300)
