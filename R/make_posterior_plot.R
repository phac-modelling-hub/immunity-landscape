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

make_england_posterior_plot <- function(post2D, obs_df, title = NULL) {
  obs_df <- obs_df %>%
    mutate(y_label = factor(location, levels = levels(post2D$y_label)))

  p <- ggplot(post2D) +
    geom_line(aes(x = x_i, y = post2D_constrained * 100, group = factor(draw)),
              alpha = 0.15, linewidth = 0.3) +
    geom_point(data = obs_df, aes(x = age_current, y = value * 100),
               colour = "red", size = 1.5, shape = 16) +
    scale_x_continuous(name = "Current age", limits = c(5, 21),
                       breaks = seq(5, 21, by = 5)) +
    scale_y_continuous(name = "Vaccine coverage (%)", limits = c(0, 100)) +
    facet_wrap(~ y_label)
  if (!is.null(title)) p <- p + ggtitle(title)
  p
}