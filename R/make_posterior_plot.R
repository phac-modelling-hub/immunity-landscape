make_posterior_plot <- function(post2D, obs_df, show_unobs = FALSE, unobs_df = NULL, cnics_df = NULL) {
  obs_df <- obs_df %>%
    mutate(y_label = factor(location, levels = levels(post2D$y_label)))

  p <- ggplot(post2D) +
    geom_point(aes(x=x_i, y=post2D_constrained, group=factor(draw)), alpha=0.1) +
    geom_line(aes(x = x_i, y = post2D_constrained , group = factor(draw)),
              alpha = 0.15, linewidth = 0.3) +
    geom_point(data = obs_df,
               aes(x = age_current, y = value, shape = n_doses),
               colour = "grey50",
              size = 2) +
    scale_shape_manual(values = shapes_doses, name = NULL, labels = c("1+" = "Training data (1+ doses)")) +
    scale_x_continuous(name = "Current age", limits = c(min(post2D$x_i), max(post2D$x_i)),
                       breaks = seq(min(post2D$x_i), max(post2D$x_i), by = 5)) +
    scale_y_continuous(name = "Vaccine coverage", labels = scales::label_percent()
    ) +
    facet_wrap(~ y_label, ncol = 3)

  if (show_unobs && !is.null(unobs_df)) {
    unobs_df <- unobs_df %>%
      mutate(y_label    = factor(location, levels = levels(post2D$y_label)),
             point_type = "Comparison (2+ doses)")
    
    overlay_df <- unobs_df
    
    if (!is.null(cnics_df)) {
      cnics_df <- cnics_df %>%
        mutate(y_label    = factor(location, levels = levels(post2D$y_label)),
               point_type = "1+ dose (cNICS)")
      overlay_df <- bind_rows(overlay_df, cnics_df)
    }
    
    p <- p +
      geom_point(data = overlay_df,
                 aes(x = age_current, y = value, colour = point_type),
                 size = 2, shape = 16) +
      scale_colour_manual(values = c("Comparison (2+ doses)" = "#00ca5eff", "1+ dose (cNICS)" = "#208ceaff"), name = NULL)
  }

  p <- p +
    guides(
      shape  = guide_legend(order = 1, override.aes = list(size = 3)),
      colour = guide_legend(order = 2, override.aes = list(size = 3))
    ) +
    theme(
      legend.position = "none",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.spacing.y = unit(0, "pt"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = unit(2, "pt")
    )
  p
}

make_england_posterior_plot <- function(post2D, obs_df = NULL, title = NULL) {

  p <- ggplot(post2D) +
    geom_line(aes(x = x_i, y = post2D_constrained , group = factor(draw)),
              alpha = 0.15, linewidth = 0.3) +
    scale_x_continuous(name = "Current age", limits = c(5, 21),
                       breaks = seq(5, 21, by = 5)) +
    scale_y_continuous(name = "Vaccine coverage"
    , labels = scales::label_percent()
    ) +
    facet_wrap(~ y_label)

  # add optional elements
  if (!is.null(obs_df)) {
    obs_df <- obs_df %>%
      mutate(y_label = factor(location, levels = levels(post2D$y_label)))

    p <- p + geom_point(data = obs_df, aes(x = age_current, y = value),
                        colour = "red", size = 1.5, shape = 16)
  }
  if (!is.null(title)) p <- p + ggtitle(title)
  p
}