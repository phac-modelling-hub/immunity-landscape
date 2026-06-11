# fig-lppd-lopo.R
# Supp Figure lppd-lopo: LOPO bar charts comparing full model vs age-only model per province.
# All three model frameworks combined into a single faceted figure.

lopo_all <- bind_rows(
  readRDS(here::here("results", "lopo_Gini.rds")) %>% mutate(model = "Gini"),
  readRDS(here::here("results", "lopo_LI.rds"))   %>% mutate(model = "Low-income"),
  readRDS(here::here("results", "lopo_VH.rds"))   %>% mutate(model = "Vaccine hesitancy")
) %>%
  mutate(model = factor(model, levels = c("Gini", "Low-income", "Vaccine hesitancy")))

fig_lopo_combined <- ggplot(lopo_all, aes(x = pt, y = diff, fill = diff > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5) +
  scale_fill_manual(
    values = colours_lopo,
    labels = c("TRUE" = "Full model preferred", "FALSE" = "Age model preferred"),
    name   = NULL) +
  facet_wrap(~ model, ncol = 1, scales = "free_x") +
  labs(x     = "PT",
       y     = "Difference in mean LPPD per PT") +
  ylim(-0.25, 0.25)

ggsave(here::here("results", "fig-lppd-lopo.pdf"),
       fig_lopo_combined, width = fig_w, height = fig_h * 2.2)
ggsave(here::here("results", "fig-lppd-lopo.jpeg"),
       fig_lopo_combined, width = fig_w, height = fig_h * 2.2, dpi = 300)
