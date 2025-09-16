#' Compute a 2D Gaussian Process prior
#' 
#' This function computes the prior for the relationship between independent variables x,y and 
#' dependent variable z.
#' @param xvals numeric vector defining possible range of x-values
#' @param yvals named numeric vector defining possible range of y-values, pre-scaled by l2, the 
#'              relative lengthscale of x values to y values
#' @param k covariance function chosen from the following list:  **Functionality not added yet.
#'          - ksqexp, 
#'          - kexp, 
#'          - kmatern.
#' @param l1 optional x-lengthscale parameter, for use with covariance functions ksqexp and kexp
#' @param ndrws optional specify number of draws from the prior distribution
#' @param prov_levels factor defining the names of provinces
#' @param b optional function scale determining the output variance for k
compute_prior2D <- function(xvals, yvals, k=ksqexp, l1=NA, ndrws=50, prov_levels, b=1) {
  # covariance matrix in 2D
  npts <- length(xvals)*length(yvals)
  covmat <- expand_grid(x1=xvals, y1=yvals, x2=xvals, y2=yvals) %>% mutate(k=k(sqrt((x1-x2)^2 + (y1-y2)^2), l1, b)) %>%
    pull(k) %>% matrix(nrow=npts)
  
  # define prior and take 50 draws
  draw_from_rmnorm(
    n = ndrws, mean = rep(0,npts), varcov = covmat + 1e-6*diag(npts), 
    xvals = xvals, yvals = yvals,
    prov_levels = prov_levels) |> 
    dplyr::rename(prior2D = value)
}