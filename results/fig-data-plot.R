# fig-data-plot.R
# Figure data-plot: observed 1+ dose vaccine coverage by province and current age.

prov_order <- names(extract_province_relation("Gini", vax_dataset = vax_clean))

obs_all <- vax_clean %>%
  mutate(y_label = factor(location, levels = prov_order))

fig_data_plot <- ggplot(obs_all, aes(x = age_current, y = value * 100, shape = n_doses)) +
  geom_point(size = 1.5, colour = "red") +
  scale_shape_manual(values = shapes_doses) +
  scale_x_continuous(name = "Current age", limits = c(min(vax_clean$age_current), max(vax_clean$age_current)),
                     breaks = seq(min(vax_clean$age_current), max(vax_clean$age_current), by = 5)) +
  scale_y_continuous(name = "Vaccine coverage (%)", limits = c(0, 100)) +
  facet_wrap(~ y_label) +
  labs(title = NULL) +
  theme(legend.position = "none")

ggsave(here::here("results", "fig-data-plot.pdf"),  fig_data_plot, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-data-plot.jpeg"), fig_data_plot, width = fig_w, height = fig_h, dpi = 300)
