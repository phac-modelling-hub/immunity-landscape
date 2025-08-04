#' Plot 2D Gaussian Process prior
#' 
#' @param df a data frame of draws from the prior with five required columns: 
#'  - draw: draw number (integer)
#'  - x_i: x-values (numeric)
#'  - y_i: y-values (named numeric)
#'  - y_label: province names corresponding to values in column y_i (factor)
#'  - prior2D: value of prior estimate at each point (x_i,y_i) (numeric)
#'
#' We can also optionally label the plot with three GP model parameters:
#' @param k covariance function name as a character string (character)
#' @param kparam1 value of first k parameter e.g. lengthscale (numeric)
#' @param kparam2 value of second k parameter (numeric)
plot_prior2D <- function(df, k_name=NA, k_param1=NA, k_param2=NA) {
  df %>% ggplot(aes(x=x_i, y=prior2D, group=factor(draw))) + scale_x_continuous(breaks=unique(df$x_i), name="current age x_i") +
    geom_line(alpha=0.3) + facet_wrap(~ y_label) + ggtitle(paste0("k=", k_name, "; k_param1=", k_param1, ", k_param2=", k_param2))
}