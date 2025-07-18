#' Calculate the squared exponential covariance function
#' 
#' @param r absolute distance between two points (abs(y1-y2))
#' @param l lengthscale
ksqexp <- function(r,l) {
  exp(-r^2 / (2*l^2))
}