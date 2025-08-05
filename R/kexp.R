#' Calculate the exponential covariance function
#' 
#' @param r absolute distance between two x vectors (abs(x1-x2))
#' @param l lengthscale
#' @param b optional function scale determining the output variance
kexp <- function(r, l, b=1) {
  b*exp(-r / l)
}