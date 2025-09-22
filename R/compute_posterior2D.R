#' Compute a 2D Gaussian Process posterior
#' 
#' This function computes the posterior for the relationship between independent variables x,y and 
#' dependent variable z, after observing data. Independent variable y is assumed to be a named variable
#' such as province.
#' @param xvals numeric vector defining possible range of x-values
#' @param yvals named numeric vector defining possible range of y-values, pre-scaled by l2, the 
#'              relative lengthscale of x-values to y-values
#' @param xobs x-coordinates of observed data (numeric)
#' @param yobs y-coordinates of observed data, pre-scaled by l2 (named numeric)
#' @param zobs z-coordinates of observed data (numeric)
#' @param k covariance function chosen from the following list:  **Functionality not added yet.
#'          - ksqexp, 
#'          - kexp, 
#'          - kmatern.
#' @param l1 optional x-lengthscale parameter, for use with covariance functions ksqexp and kexp
#' @param ndrws optional specify number of draws from the prior distribution
#' @param prov_levels factor defining the names of provinces e.g. for y-values
#' @param b optional function scale determining the output variance for k
compute_posterior2D <- function(xvals, yvals, xobs, yobs, zobs, k=ksqexp, l1=NA, ndrws=50, prov_levels, b=1) {
  # calculate covariances between unobserved and observed
  xygrid <- expand_grid(x=xvals, y=yvals)
  xyobs <- tibble(x=xobs,y=yobs)
  
  kuo <- generate_2Dksqexp_covmat(xygrid,xyobs,fn=k,l=l1,b=b)  # 'relationship' pairwise between unobserved and observed
  kou <- generate_2Dksqexp_covmat(xyobs,xygrid,fn=k,l=l1,b=b)  # 'relationship' pairwise between observed and unobserved
  koo <- generate_2Dksqexp_covmat(xyobs,xyobs,fn=k,l=l1,b=b) + 1e-6*diag(nrow(xyobs))  # protect against non-invertibleness
  kuu <- generate_2Dksqexp_covmat(xygrid,xygrid,fn=k,l=l1,b=b)
  
  # calculate posterior mean and posterior cov matrix
  logit_zobs_centred <- logit(zobs) - mean(logit(zobs))  # use the logistic-transformed data centred around mean 0
  post_mean <- kuo%*%solve(koo)%*%(logit_zobs_centred)  # conditional mean
  post_covmat <- kuu - (kuo%*%solve(koo)%*%kou)  # conditional variance
  
  # draw from the posterior distribution
  draw_from_rmnorm(
    n = ndrws,
    mean = post_mean,
    varcov = post_covmat + 1e-6*diag(nrow(post_covmat)),
    xvals = xvals,
    yvals = yvals,
    prov_levels = prov_levels
  ) |>
    dplyr::rename(post2D = value) |>
    dplyr::mutate(post2D_constrained = invlogit(post2D + mean(logit(zobs)))) |>
    dplyr::relocate(post2D, .after = y_label)
}