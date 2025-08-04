#' Plot 2D Gaussian Process posterior
#' 
#' This plotting code outputs up to three posterior plots: posterior on [-inf,inf], and the inverse-logit transformed
#' posterior on [0,1] with and without additional unobserved data points added for comparison.  **Functionality 
#' not yet added for third plot.
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
#' We can also optionally label the plot with three GP model parameters:
#' @param k_name covariance function name as a character string (character)
#' @param kparam1 value of first k parameter e.g. lengthscale (numeric)
#' @param kparam2 value of second k parameter (numeric)
plot_posterior2D <- function(df, xobs, yobs, zobs, vax_clean=NA, last_agecurrent=NA, k_name=NA, k_param1=NA, k_param2=NA) {
  yobs_factors <- factor(names(yobs), levels(df$y_label))  # convert yobs to factors for use in facet_wrap
  
  #' plot 1
  p1 <- df %>% ggplot() + geom_point(aes(x=x_i, y=post2D, group=factor(draw)), alpha=0.1) +
    geom_line(aes(x=x_i, y=post2D, group=factor(draw)), alpha=0.2) +
    geom_point(data=tibble(x=xobs, y=logit(zobs), y_label=yobs_factors), aes(x=x, y=y), col="red") +
    scale_x_continuous(breaks=unique(df$x_i), name="current age x_i") +
    ggtitle(paste0("k=", k_name, "; k_param1=", k_param1, ", k_param2=", k_param2)) +
    facet_wrap(~ y_label)
  
  #' plot 2
  p2 <- df %>% ggplot() + geom_point(aes(x=x_i, y=post2D_constrained, group=factor(draw)), alpha=0.1) +
    geom_line(aes(x=x_i, y=post2D_constrained, group=factor(draw)), alpha=0.2) +
    geom_point(data=tibble(x=xobs, y=zobs, y_label=yobs_factors), aes(x=x, y=y), col="red") +
    scale_x_continuous(breaks=unique(df$x_i), name="current age x_i") +
    ggtitle(paste0("k=", k_name, "; k_param1=", k_param1, ", k_param2=", k_param2)) +
    facet_wrap(~ y_label) 
  
  #' #' plot 3
  #' df %>% ggplot() + geom_point(aes(x=x_i, y=post2D_constrained, group=factor(draw)), alpha=0.1) + 
  #'   geom_line(aes(x=x_i, y=post2D_constrained, group=factor(draw)), alpha=0.2) + 
  #'   geom_point(data=tibble(x=(vax_clean %>% filter(age_current<=last_agecurrent) %>% pull(age_current)),  # adding in full data from vax_clean
  #'                          y=(vax_clean %>% filter(age_current<=last_agecurrent) %>% pull(value)), 
  #'                          y_label=factor((vax_clean %>% filter(age_current<=last_agecurrent) %>% pull(location)), levels=prov_levels)),
  #'              aes(x=x,y=y), col="green") + 
  #'   geom_point(data=tibble(x=xobs, y=zobs, y_label=yobs_factors), aes(x=x, y=y), col="red") +
  #'   scale_x_continuous(breaks=unique(df$x_i), name="current age x_i") + #scale_y_continuous(limits=c(0,1))
  #'   facet_wrap(~ y_label)
  
  list(p1,p2)
}