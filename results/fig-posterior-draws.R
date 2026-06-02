# fig-posterior-draws.R
# Figure posterior-draws: posterior draws for Gini, LI, and VH models.
# Each model is a separate row in the plot.

post_Gini <- readRDS(here::here("results", "posterior_Gini.rds"))
post_LI   <- readRDS(here::here("results", "posterior_LI.rds"))
post_VH   <- readRDS(here::here("results", "posterior_VH.rds"))

obs_Gini <- vax_clean   %>% filter(age_current >= first_agecurrent, age_current <= last_agecurrent, n_doses != "2+")
obs_LI   <- vax_cleanLI %>% filter(age_current >= first_agecurrent, age_current <= last_agecurrent, n_doses != "2+")
obs_VH   <- vax_cleanVH %>% filter(age_current >= first_agecurrent, age_current <= last_agecurrent, n_doses != "2+")

make_posterior_plot <- function(post2D, obs_df, show_unobs = FALSE, unobs_df = NULL) {
  obs_df <- obs_df %>%
    mutate(y_label = factor(location, levels = levels(post2D$y_label)))

  p <- ggplot(post2D) +
    geom_line(aes(x = x_i, y = post2D_constrained * 100, group = factor(draw)),
              alpha = 0.15, linewidth = 0.3) +
    geom_point(data = obs_df,
               aes(x = age_current, y = value * 100, shape = n_doses),
               colour = "red", size = 1.2) +
    scale_shape_manual(values = shapes_doses, name = "Doses") +
    scale_x_continuous(name = "Current age", limits = c(first_agecurrent, last_agecurrent),
                       breaks = seq(5, 21, by = 5)) +
    scale_y_continuous(name = "Vaccine coverage (%)", limits = c(0, 100)) +
    facet_wrap(~ y_label, nrow = 1)

  if (show_unobs && !is.null(unobs_df)) {
    unobs_df <- unobs_df %>%
      mutate(y_label    = factor(location, levels = levels(post2D$y_label)),
             point_type = "2+")
    p <- p +
      geom_point(data = unobs_df,
                 aes(x = age_current, y = value * 100, colour = point_type),
                 size = 1.2, shape = 16) +
      scale_colour_manual(values = c("2+" = "#1b7837"), name = NULL)
  }
  p
}

fig_posterior_Gini <- make_posterior_plot(post_Gini, obs_Gini)  #need to combine into one here
fig_posterior_LI   <- make_posterior_plot(post_LI,   obs_LI)
fig_posterior_VH   <- make_posterior_plot(post_VH,   obs_VH)

ggsave(here::here("results", "fig-posterior-draws_Gini.pdf"),
       fig_posterior_Gini, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-posterior-draws_LI.pdf"),
       fig_posterior_LI, width = fig_w, height = fig_h)
ggsave(here::here("results", "fig-posterior-draws_VH.pdf"),
       fig_posterior_VH, width = fig_w, height = fig_h)

# Combine into one plot with patchwork package
library(patchwork)
n_Gini <- nlevels(post_Gini$y_label)
n_LI   <- nlevels(post_LI$y_label)
n_VH   <- nlevels(post_VH$y_label)
max_n  <- max(n_Gini, n_LI, n_VH)

pad_plot <- function(p, n) {
  if (n < max_n) {
    p + plot_spacer() + plot_layout(widths = c(n, max_n - n))
  } else {
    p
  }
}

fig_posterior_combined <- pad_plot(fig_posterior_Gini, n_Gini) / pad_plot(fig_posterior_LI, n_LI) / pad_plot(fig_posterior_VH, n_VH)
ggsave(here::here("results", "fig-posterior-draws.pdf"),
       fig_posterior_combined, width = fig_w, height = fig_h * 3)

# ── Gini with unobserved overlay (2+ dose data and cNICS) ────────────────────────────────
unobs_Gini <- vax_clean %>%  #need to add cnics here
  filter(age_current >= first_agecurrent, age_current <= last_agecurrent, n_doses == "2+") %>%
  select(age_current, location, value)

fig_posterior_Gini_unobs <- make_posterior_plot(post_Gini, obs_Gini, show_unobs = TRUE, unobs_df = unobs_Gini)
ggsave(here::here("results", "fig-posterior-draws_Gini_unobs.pdf"),
       fig_posterior_Gini_unobs, width = fig_w, height = fig_h)
