#' Calculate the squared exponential covariance function
#' 
#' @param r absolute distance between two x vectors of interest (abs(x1-x2))
#' @param l lengthscale
ksqexp <- function(r,l) {
  exp(-r^2 / (2*l^2))
}