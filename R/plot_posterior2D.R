#' Plot 2D Gaussian Process posterior
#' 
#' This plotting code outputs up to three posterior plots: posterior on [-inf,inf], and the inverse-logit transformed
#' posterior on [0,1] with and without additional unobserved data points added for comparison.
#' @param df a data frame of draws from the posterior with six required columns: 
#'  - draw: draw number (integer)
#'  - x_i: x-values (numeric)
#'  - y_i: y-values (named numeric)
#'  - y_label: province names corresponding to values in column y_i (factor)
#'  - post2D: value of posterior estimate at each point (x_i,y_i) (numeric)
#'  - post2D_constrained: inverse-logit transformed value of posterior estimate at each point (x_i,y_i) (numeric)
#' @param xobs x-coordinates of observed data (numeric)
#' @param yobs y-coordinates of observed data, pre-scaled by l2 (named numeric)
#' @param zobs z-coordinates of observed data (numeric)
#' 
#' We can also optionally label the plot:
#' @param k_param1 value of first k parameter e.g. lengthscale (numeric)
#' @param k_param2 value of second k parameter e.g. relative lengthscale of x to y (numeric)
#' @param k_param3 value of third k parameter e.g. b (numeric)
#' 
#' The final plot also makes use of the full (observed and unobserved) data from vax_dataset:
#' @param vax_dataset data set of observed vaccine coverage data in our standardised format (tibble)
#' @param prov_levels factor defining the names of provinces
plot_posterior2D <- function(df, xobs, yobs, zobs, k_param1=NA, k_param2=NA, k_param3=NA,
                             vax_dataset=NA, prov_levels=NA) {
  yobs_factors <- factor(names(yobs), levels(df$y_label))  # convert yobs to factors for use in facet_wrap

  p1 <- df %>% ggplot() + geom_point(aes(x=x_i, y=post2D_constrained, group=factor(draw)), alpha=0.1) +
    geom_line(aes(x=x_i, y=post2D_constrained, group=factor(draw)), alpha=0.2) +
    geom_point(data=tibble(x=xobs, y=zobs, y_label=yobs_factors), aes(x=x, y=y), col="red") +
    scale_x_continuous(limits=c(min(df$x_i), max(df$x_i)), breaks=seq(min(df$x_i), max(df$x_i), by=5), name="Current age") + 
    scale_y_continuous(
      limits=c(0,1), 
      labels = scales::label_percent(), 
      name = "Vaccine coverage") + 
    labs(
      title = "Posterior draws",
      subtitle = paste0("Covariance function parameters: l1=", k_param1, ", l2=", k_param2)) +
    facet_wrap(~ y_label)
  
  p2 <- df %>% ggplot() + geom_point(aes(x=x_i, y=post2D_constrained, group=factor(draw)), alpha=0.1) +
    geom_line(aes(x=x_i, y=post2D_constrained, group=factor(draw)), alpha=0.2) +
    geom_point(data=tibble(x=(vax_dataset %>% pull(age_current)),  # adding in full data from vax_clean
                           y=((vax_dataset %>% pull(value))),
                           y_label=factor((vax_dataset %>% pull(location)), levels=prov_levels)),
               aes(x=x,y=y), col="#208ceaff") +
    geom_point(data=tibble(x=xobs, y=zobs, y_label=yobs_factors), aes(x=x, y=y), col="red") +
    scale_x_continuous(limits=c(min(df$x_i), max(df$x_i)), breaks=seq(min(df$x_i), max(df$x_i), by=5), name="Current age") + 
    scale_y_continuous(
      limits=c(0,1), 
      labels = scales::label_percent(), 
      name = "Vaccine coverage") + 
    labs(
      title = "Posterior draws",
      subtitle = paste0("Covariance function parameters: l1=", k_param1, ", l2=", k_param2)) +
    facet_wrap(~ y_label)
  
  list(p1,p2)
}