# fig-data-plot.R
# Figure data-plot: observed 1+ dose vaccine coverage by province and current age.

prov_order <- names(extract_province_relation("Gini", vax_dataset = vax_clean))

obs_all <- vax_clean %>%
  filter(age_current >= first_agecurrent, age_current <= last_agecurrent, n_doses != "2+") %>%
  mutate(y_label = factor(location, levels = prov_order))

fig_data_plot <- ggplot(obs_all, aes(x = age_current, y = value * 100, shape = n_doses)) +
  geom_point(size = 1.5, colour = "red") +
  scale_shape_manual(values = shapes_doses) +
  scale_x_continuous(name = "Current age", limits = c(first_agecurrent, last_agecurrent),
                     breaks = seq(5, 21, by = 5)) +
  scale_y_continuous(name = "Vaccine coverage (%)", limits = c(0, 100)) +
  facet_wrap(~ y_label) +
  labs(title = NULL) +
  theme(legend.position = "none")

ggsave(here::here("results", "fig-data-plot.pdf"),  fig_data_plot, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-data-plot.jpeg"), fig_data_plot, width = fig_w, height = fig_h, dpi = 300)
