# fig-posterior-draws.R
# Figure posterior-draws: posterior draws for Gini, LI, and VH models.
# Each model is a separate row in the plot.

post_Gini <- readRDS(here::here("results", "posterior_Gini.rds"))
post_LI   <- readRDS(here::here("results", "posterior_LI.rds"))
post_VH   <- readRDS(here::here("results", "posterior_VH.rds"))

obs_Gini <- vax_clean
obs_LI   <- vax_cleanLI
obs_VH   <- vax_cleanVH

fig_posterior_Gini <- make_posterior_plot(post_Gini, obs_Gini)
fig_posterior_LI   <- make_posterior_plot(post_LI,   obs_LI)
fig_posterior_VH   <- make_posterior_plot(post_VH,   obs_VH)

ggsave(here::here("results", "fig-posterior-draws_LI.pdf"),
       fig_posterior_LI, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-posterior-draws_VH.pdf"),
       fig_posterior_VH, width = fig_w, height = fig_h)

# Combine into one plot with facet_grid
# Province ordering: Gini order first, then any VH-unique region names appended
gini_order     <- levels(post_Gini$y_label)
vh_unique      <- setdiff(levels(post_VH$y_label), gini_order)
prov_order_all <- c(gini_order, vh_unique)

post_all <- bind_rows(
  post_Gini %>% mutate(model = "Gini",              province = as.character(y_label)),
  post_LI   %>% mutate(model = "Low-income",        province = as.character(y_label)),
  post_VH   %>% mutate(model = "Vaccine hesitancy", province = as.character(y_label))
) %>%
  mutate(
    model    = factor(model, levels = c("Gini", "Low-income", "Vaccine hesitancy")),
    province = factor(province, levels = prov_order_all)
  )

obs_all <- bind_rows(
  obs_Gini %>% mutate(model = "Gini",              province = location),
  obs_LI   %>% mutate(model = "Low-income",        province = location),
  obs_VH   %>% mutate(model = "Vaccine hesitancy", province = location)
) %>%
  mutate(
    model    = factor(model, levels = c("Gini", "Low-income", "Vaccine hesitancy")),
    province = factor(province, levels = prov_order_all)
  )

pt_labels <- pt_lookup() %>% select(location, pt) %>% deframe()
pt_labels["Atlantic region"] <- "AR"
pt_labels["Northern region"] <- "NR"

fig_posterior_combined <- ggplot(post_all) +
  geom_line(aes(x = x_i, y = post2D_constrained * 100,
                group = interaction(draw, province)),
            alpha = 0.15, linewidth = 0.3) +
  geom_point(data = obs_all,
             aes(x = age_current, y = value * 100, shape = n_doses),
             colour = "red", size = 1.2) +
  scale_shape_manual(values = shapes_doses, name = "Doses") +
  scale_x_continuous(name = "Current age", limits = c(min(post_all$x_i), max(post_all$x_i)),
                     breaks = seq(min(post_all$x_i), max(post_all$x_i), by = 5)) +
  scale_y_continuous(name = "Vaccine coverage (%)") +
  facet_grid(model ~ province, scales = "free_x", space = "free_x",
             labeller = labeller(province = as_labeller(pt_labels))) +
  theme(legend.position = "none")

ggsave(here::here("results", "fig-posterior-draws.pdf"),
       fig_posterior_combined, width = fig_w * 2, height = fig_h * 1.5)

# ── Gini with unobserved overlay (2+ dose data) ────────────────────────────────
unobs_Gini <- readr::read_csv(here::here("data", "measles_vax-coverage-data-cleaned_2plus.csv")) %>%
  select(age_current, location, value)

fig_posterior_Gini_unobs <- make_posterior_plot(post_Gini, obs_Gini, show_unobs = TRUE, unobs_df = unobs_Gini) +
  labs(title = "Posterior draws from model fit") +
  theme(legend.position = "inside",
    legend.position.inside = c(0.98, -0.05),   # bottom-right
    legend.justification = c(1, 0)
)
fig_posterior_Gini_unobs

ggsave(here::here("results", "fig-posterior-draws_Gini_unobs.jpeg"),
       fig_posterior_Gini_unobs, width = fig_w, height = fig_h, dpi = 300)
ggsave(here::here("results", "fig-posterior-draws_Gini_unobs.pdf"),
       fig_posterior_Gini_unobs, width = fig_w, height = fig_h)
