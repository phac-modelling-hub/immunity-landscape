# fig-provinces-test.R
# Supp Figure provinces-test: SES indicator value vs mean 1+ dose coverage by province.

vax_means_prov <- vax_clean %>%
  filter(age_current >= 5, age_current <= 21, n_doses != "2+") %>%
  group_by(location, pt) %>%
  summarise(mean_coverage = mean(value), .groups = "drop")

make_prov_scatter <- function(indicator_vec, means_df, x_label) {
  tibble(location  = names(indicator_vec),
         indicator = as.numeric(indicator_vec)) %>%
    left_join(means_df, by = "location") %>%
    ggplot(aes(x = indicator, y = mean_coverage * 100, label = pt)) +
    geom_point(size = 2) +
    ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
    scale_y_continuous(name = "Mean 1+ dose coverage (%)", limits = c(0, 100)) +
    labs(x = x_label)
}

fig_prov_Gini <- make_prov_scatter(
  extract_province_relation("Gini",               vax_dataset = vax_clean),
  vax_means_prov, "Gini index"
)
fig_prov_LI <- make_prov_scatter(
  extract_province_relation("low_income_families", vax_dataset = vax_cleanLI),
  vax_means_prov, "Low-income families (%)"
)
fig_prov_VH <- make_prov_scatter(
  extract_province_relation("vaccine_hesitancy",   vax_dataset = vax_cleanVH),
  vax_means_prov, "Vaccine hesitancy index"
)

fig_provinces_test <- fig_prov_Gini | fig_prov_LI | fig_prov_VH

ggsave(here::here("results", "fig-provinces-test.pdf"),
       fig_provinces_test, width = fig_w * 1.5, height = fig_h)
ggsave(here::here("results", "fig-provinces-test.jpeg"),
       fig_provinces_test, width = fig_w * 1.5, height = fig_h, dpi = 300)
