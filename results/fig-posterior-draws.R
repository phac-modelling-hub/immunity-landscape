# fig-posterior-draws.R
# Figure posterior-draws: posterior draws for Gini, LI, and VH models,
# with optional overlay of unobserved/2+ data in green.

post_Gini <- readRDS(here::here("results", "posterior_Gini.rds"))
post_LI   <- readRDS(here::here("results", "posterior_LI.rds"))
post_VH   <- readRDS(here::here("results", "posterior_VH.rds"))

obs_Gini <- vax_clean   %>% filter(age_current >= 5, age_current <= 21, n_doses != "2+")
obs_LI   <- vax_cleanLI %>% filter(age_current >= 5, age_current <= 21, n_doses != "2+")
obs_VH   <- vax_cleanVH %>% filter(age_current >= 5, age_current <= 21, n_doses != "2+")

make_posterior_plot <- function(post2D, obs_df, show_unobs = FALSE, unobs_df = NULL) {
  p <- ggplot(post2D) +
    geom_line(aes(x = x_i, y = post2D_constrained * 100, group = factor(draw)),
              alpha = 0.15, linewidth = 0.3) +
    geom_point(data = obs_df,
               aes(x = age_current, y = value * 100, shape = n_doses),
               colour = "red", size = 1.2) +
    scale_shape_manual(values = shapes_doses, name = "Doses") +
    scale_x_continuous(name = "Current age", limits = c(5, 21),
                       breaks = seq(5, 21, by = 5)) +
    scale_y_continuous(name = "Vaccine coverage (%)", limits = c(0, 100)) +
    facet_wrap(~ y_label)

  if (show_unobs && !is.null(unobs_df)) {
    p <- p + geom_point(data = unobs_df,
                        aes(x = age_current, y = value * 100),
                        colour = "#1b7837", size = 1.2, shape = 16)
  }
  p
}

# ── Gini ──────────────────────────────────────────────────────────────────────
fig_posterior_Gini <- make_posterior_plot(post_Gini, obs_Gini)
ggsave(here::here("results", "fig-posterior-draws_Gini.pdf"),
       fig_posterior_Gini, width = fig_w, height = fig_h)

# Gini with unobserved overlay (2+ dose data)
unobs_Gini <- vax_clean %>%
  filter(age_current >= 5, age_current <= 21, n_doses == "2+") %>%
  select(age_current, location, value)
fig_posterior_Gini_unobs <- make_posterior_plot(post_Gini, obs_Gini,
                                                show_unobs = TRUE, unobs_df = unobs_Gini)
ggsave(here::here("results", "fig-posterior-draws_Gini_unobs.pdf"),
       fig_posterior_Gini_unobs, width = fig_w, height = fig_h)

# ── LI ────────────────────────────────────────────────────────────────────────
fig_posterior_LI <- make_posterior_plot(post_LI, obs_LI)
ggsave(here::here("results", "fig-posterior-draws_LI.pdf"),
       fig_posterior_LI, width = fig_w, height = fig_h)

# ── VH ────────────────────────────────────────────────────────────────────────
fig_posterior_VH <- make_posterior_plot(post_VH, obs_VH)
ggsave(here::here("results", "fig-posterior-draws_VH.pdf"),
       fig_posterior_VH, width = fig_w, height = fig_h)
