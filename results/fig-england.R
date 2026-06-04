# fig-england.R
# Supp England figures: England model posteriors and LPPD heatmaps (Gini and LI).

post_england_Gini <- readRDS(here::here("results", "posterior_england_Gini.rds"))
post_england_LI   <- readRDS(here::here("results", "posterior_england_LI.rds"))

current_year <- as.integer(format(Sys.Date(), "%Y"))
england_obs <- read_csv(here::here("data", "ukhsa-chart-download-mmr1-regions.csv"),
                        show_col_types = FALSE) %>%
  mutate(age         = 5,
         n_doses     = "1+",
         value       = metric_value / 100,
         location    = geography,
         year_report = year,
         age_current = current_year - year_report + age) %>%
  filter(age_current >= 5, age_current <= 21) %>%
  select(location, age_current, value, n_doses)

# ── Posterior plots ────────────────────────────────────────────────────────────

fig_england_posterior_Gini <- make_england_posterior_plot(
  post_england_Gini, england_obs, "England — Gini model posterior"
)
fig_england_posterior_LI <- make_england_posterior_plot(
  post_england_LI, england_obs, "England — Low-income model posterior"
)

ggsave(here::here("results", "fig-england-posterior_Gini.pdf"),
       fig_england_posterior_Gini, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-england-posterior_LI.pdf"),
       fig_england_posterior_LI, width = fig_w, height = fig_h)

# ── LPPD heatmaps ─────────────────────────────────────────────────────────────
make_england_heatmap <- function(csv_name, title_lab) {
  read_csv(here::here("results", csv_name), show_col_types = FALSE) %>%
    filter(k_name == "ksqexp", b == 0.5) %>%
    mutate(l1 = factor(l1), l2 = factor(l2),
           lppd_capped = if_else(lppd_exact < -100, NA_real_, lppd_exact)) %>%
    ggplot(aes(x = l1, y = l2, fill = lppd_capped)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "#d73027", na.value = "white",
                        name = "LOO lppd") +
    facet_wrap(~ meas_error, labeller = label_both) +
    labs(title = paste0("LPPD heatmap — England ", title_lab, " model"), x = "l1", y = "l2")
}

fig_england_heatmap_Gini <- make_england_heatmap("GPcombinations_englandGini_witherror.csv", "Gini")
fig_england_heatmap_LI   <- make_england_heatmap("GPcombinations_englandLI_witherror.csv",   "Low-income")

ggsave(here::here("results", "fig-england-heatmap_Gini.pdf"),
       fig_england_heatmap_Gini, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-england-heatmap_LI.pdf"),
       fig_england_heatmap_LI, width = fig_w, height = fig_h)
