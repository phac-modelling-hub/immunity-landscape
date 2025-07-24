#' Calculate the linear covariance function
#' 
#' @param x1 random vector 1 of model inputs x1_i (numeric vector)
#' @param x2 random vector 2 of model inputs x2_i (numeric vector)
#' The line is then given by:
#' @param sigma_int (square root of) y-intercept
#' @param sigma_slope (square root of) gradient
#' 
#' Note that using a linear covariance function in a Gaussian Process just collapses to Bayesian linear regression.

klin <- function(x1, x2, sigma_int, sigma_slope){  # linear covariance function
  sigma_int^2 + (sigma_slope^2)*(x1*x2)
}