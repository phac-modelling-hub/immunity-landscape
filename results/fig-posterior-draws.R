# fig-posterior-draws.R
# Figure posterior-draws: posterior draws for Gini, LI, and VH models.
# Each model is a separate row in the plot.

post_Gini <- readRDS(here::here("results", "posterior_Gini.rds"))
post_LI   <- readRDS(here::here("results", "posterior_LI.rds"))
post_VH   <- readRDS(here::here("results", "posterior_VH.rds"))

obs_Gini <- vax_clean   %>% filter(age_current >= first_agecurrent, age_current <= last_agecurrent, n_doses != "2+")
obs_LI   <- vax_cleanLI %>% filter(age_current >= first_agecurrent, age_current <= last_agecurrent, n_doses != "2+")
obs_VH   <- vax_cleanVH %>% filter(age_current >= first_agecurrent, age_current <= last_agecurrent, n_doses != "2+")

make_posterior_plot <- function(post2D, obs_df, show_unobs = FALSE, unobs_df = NULL, cnics_df = NULL) {
  obs_df <- obs_df %>%
    mutate(y_label = factor(location, levels = levels(post2D$y_label)))

  p <- ggplot(post2D) +
    geom_point(aes(x=x_i, y=post2D_constrained*100, group=factor(draw)), alpha=0.1) +
    geom_line(aes(x = x_i, y = post2D_constrained * 100, group = factor(draw)),
              alpha = 0.15, linewidth = 0.3) +
    geom_point(data = obs_df,
               aes(x = age_current, y = value * 100, shape = n_doses),
               colour = "red") +
    scale_shape_manual(values = shapes_doses, name = NULL, labels = c("1+" = "1+ dose (provincial)")) +
    scale_x_continuous(name = "Current age", limits = c(first_agecurrent, last_agecurrent),
                       breaks = seq(5, 21, by = 5)) +
    scale_y_continuous(name = "Vaccine coverage (%)", limits = c(0, 100)) +
    facet_wrap(~ y_label)

  if (show_unobs && !is.null(unobs_df)) {
    unobs_df <- unobs_df %>%
      mutate(y_label    = factor(location, levels = levels(post2D$y_label)),
             point_type = "2+ dose (provincial)")
    
    overlay_df <- unobs_df
    
    if (!is.null(cnics_df)) {
      cnics_df <- cnics_df %>%
        mutate(y_label    = factor(location, levels = levels(post2D$y_label)),
               point_type = "1+ dose (cNICS)")
      overlay_df <- bind_rows(overlay_df, cnics_df)
    }
    
    p <- p +
      geom_point(data = overlay_df,
                 aes(x = age_current, y = value * 100, colour = point_type),
                 size = 1.2, shape = 16) +
      scale_colour_manual(values = c("2+ dose (provincial)" = "green", "1+ dose (cNICS)" = "lightgreen"), name = NULL)
  }
  
  p <- p + theme(legend.position = "none")
  p
}

fig_posterior_Gini <- make_posterior_plot(post_Gini, obs_Gini)
fig_posterior_LI   <- make_posterior_plot(post_LI,   obs_LI)
fig_posterior_VH   <- make_posterior_plot(post_VH,   obs_VH)

ggsave(here::here("results", "fig-posterior-draws_Gini.pdf"),
       fig_posterior_Gini, width = fig_w, height = fig_h)
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

fig_posterior_combined <- ggplot(post_all) +
  geom_line(aes(x = x_i, y = post2D_constrained * 100,
                group = interaction(draw, province)),
            alpha = 0.15, linewidth = 0.3) +
  geom_point(data = obs_all,
             aes(x = age_current, y = value * 100, shape = n_doses),
             colour = "red", size = 1.2) +
  scale_shape_manual(values = shapes_doses, name = "Doses") +
  scale_x_continuous(name = "Current age", limits = c(first_agecurrent, last_agecurrent),
                     breaks = seq(5, 21, by = 5)) +
  scale_y_continuous(name = "Vaccine coverage (%)", limits = c(0, 100)) +
  facet_grid(model ~ province, scales = "free_x", space = "free_x") +
  theme(legend.position = "none")

ggsave(here::here("results", "fig-posterior-draws.pdf"),
       fig_posterior_combined, width = fig_w * 2, height = fig_h * 1.5)

# ── Gini with unobserved overlay (2+ dose data and cNICS) ────────────────────────────────
unobs_Gini <- vax_clean %>%  #need to add cnics here
  filter(age_current >= first_agecurrent, age_current <= last_agecurrent, n_doses == "2+") %>%
  select(age_current, location, value)

cnics_Gini <- vax_cNICS %>%
  filter(age_current >= first_agecurrent, age_current <= last_agecurrent,
         n_doses == "1+") %>%
  select(age_current, location, value)

fig_posterior_Gini_unobs <- make_posterior_plot(post_Gini, obs_Gini, show_unobs = TRUE, unobs_df = unobs_Gini, cnics_df = cnics_Gini) +
  theme(legend.position = "bottom")
ggsave(here::here("results", "fig-posterior-draws_Gini_unobs.pdf"),
       fig_posterior_Gini_unobs, width = fig_w, height = fig_h)