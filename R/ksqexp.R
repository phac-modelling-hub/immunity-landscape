#' Calculate the squared exponential covariance function
#' 
#' @param r absolute distance between two x vectors of interest (abs(x1-x2))
#' @param l lengthscale
#' @param b optional function scale determining the output variance
ksqexp <- function(r, l, b=1) {
  b*exp(-r^2 / (2*l^2))
}