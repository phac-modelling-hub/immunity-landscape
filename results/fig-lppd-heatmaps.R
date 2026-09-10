# fig-lppd-heatmaps.R
# Supp Figure lppd-heatmaps: LPPD heatmaps over (l2, l1) for Canada models
fig_heatmaps <- list(read_csv(here::here("results", "GPcombinations_lowincome_witherror.csv"))      %>% mutate(model = "LI"),
                    read_csv(here::here("results", "GPcombinations_vaccinehesitancy_witherror.csv")) %>% mutate(model = "VH"),
                    read_csv(here::here("results", "GPcombinations_Gini_witherror.csv"))           %>% mutate(model = "Gini"),
                    read_csv(here::here("results", "GPcombinations_CU_witherror.csv"))             %>% mutate(model = "CU")) %>%
  bind_rows() %>%
  filter(b == 1, !l2 %in% c(1.5, 2.5, 3.5, 5)) %>%
  mutate(l1 = factor(l1), l2 = factor(l2),
         lppd_capped = if_else(lppd_exact < -100, NA_real_, lppd_exact)) %>%
  ggplot(aes(x = l2, y = l1, fill = lppd_capped)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "white", name = "lppd > -100") +
  facet_grid(rows = vars(model, k_name), cols = vars(meas_error), scales = "free_x",
             labeller = labeller(meas_error = function(x) paste0("psi = ", x))) +
  labs(x = "l2", y = "l1") +
  theme(axis.text = element_text(size = 6),
        axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5))
ggsave(here::here("results", "fig-lppd-heatmaps.pdf"),   fig_heatmaps,   width = fig_w, height = fig_h)