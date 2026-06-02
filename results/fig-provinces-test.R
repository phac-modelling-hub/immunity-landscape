# fig-provinces-test.R
# Supp Figure provinces-test: SES indicator value vs mean 1+ dose coverage by province.
# All three indicators combined into a single faceted figure.

vax_means_prov <- vax_clean %>%
  filter(age_current >= 5, age_current <= 21, n_doses != "2+") %>%
  group_by(location, pt) %>%
  summarise(mean_coverage = mean(value), .groups = "drop")

gini_vec <- extract_province_relation("Gini",               vax_dataset = vax_clean)
li_vec   <- extract_province_relation("low_income_families", vax_dataset = vax_cleanLI)
vh_vec   <- extract_province_relation("vaccine_hesitancy",   vax_dataset = vax_cleanVH)

prov_all <- bind_rows(
  tibble(location = names(gini_vec), indicator = as.numeric(gini_vec),
         model = "Gini", x_label = "Gini index"),
  tibble(location = names(li_vec),   indicator = as.numeric(li_vec),
         model = "Low-income", x_label = "Low-income families (%)"),
  tibble(location = names(vh_vec),   indicator = as.numeric(vh_vec),
         model = "Vaccine hesitancy", x_label = "Vaccine hesitancy index")
) %>%
  mutate(model = factor(model, levels = c("Gini", "Low-income", "Vaccine hesitancy"))) %>%
  left_join(vax_means_prov, by = "location")  # note this plot needs a value for Atlantic Region and Northern Region to be added, but we are planning to scrap the plot anyway!

fig_provinces_test <- ggplot(prov_all,
                             aes(x = indicator, y = mean_coverage * 100, label = pt)) +
  geom_point(size = 2) +
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20) +
  scale_y_continuous(name = "Mean 1+ dose coverage (%)", limits = c(70, 100)) +
  facet_wrap(~ model, scales = "free_x", nrow = 1,
             labeller = labeller(model = function(x) {
               c("Gini"              = "Gini index",
                 "Low-income"        = "Percentage of children not in low-income families (%)",
                 "Vaccine hesitancy" = "Percentage of parents who are not vaccine-hesitant (%)")[x]
             })) +
  labs(x = NULL)

ggsave(here::here("results", "fig-provinces-test.pdf"),
       fig_provinces_test, width = fig_w * 1.5, height = fig_h)
ggsave(here::here("results", "fig-provinces-test.jpeg"),
       fig_provinces_test, width = fig_w * 1.5, height = fig_h, dpi = 300)