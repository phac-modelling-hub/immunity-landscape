# fig-data-plot.R
# Figure data-plot: observed 1+ dose vaccine coverage by province and current age.

prov_order <- names(extract_province_relation("Gini", vax_dataset = vax_clean))

obs_all <- vax_clean %>%
  filter(age_current >= 5, age_current <= 21, n_doses != "2+") %>%
  mutate(y_label = factor(location, levels = prov_order))

fig_data_plot <- ggplot(obs_all, aes(x = age_current, y = value * 100, shape = n_doses)) +
  geom_point(size = 1.5, colour = "#444444") +
  scale_shape_manual(values = shapes_doses, name = "Doses") +
  scale_x_continuous(name = "Current age", limits = c(5, 21),
                     breaks = seq(5, 21, by = 5)) +
  scale_y_continuous(name = "Vaccine coverage (%)", limits = c(0, 100)) +
  facet_wrap(~ y_label) +
  labs(title = NULL)

ggsave(here::here("results", "fig-data-plot.pdf"),  fig_data_plot, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-data-plot.jpeg"), fig_data_plot, width = fig_w, height = fig_h, dpi = 300)
