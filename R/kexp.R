#' Calculate the exponential covariance function
#' 
#' @param r absolute distance between two x vectors (abs(x1-x2))
#' @param l lengthscale
ksqexp <- function(r,l) {
  exp(-r / l)
}