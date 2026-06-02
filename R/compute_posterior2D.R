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
#' @param meas_error if specified, measurement error is included (numeric)
compute_posterior2D <- function(xvals, yvals, xobs, yobs, zobs, k=ksqexp, l1=NA, ndrws=50, prov_levels, b=1, meas_error=NA) {

  fit <- fit_GP2(xvals, yvals, xobs, yobs, zobs, k=k, l1=l1, b=b, meas_error=meas_error)

  # draw from the posterior distribution
  draw_from_rmnorm(
    n = ndrws,
    mean = fit$post_mean,
    varcov = fit$post_covmat + 1e-6*diag(nrow(fit$post_covmat)),
    xvals = xvals,
    yvals = yvals,
    prov_levels = prov_levels
  ) |>
    dplyr::rename(post2D = value) |>
    dplyr::mutate(post2D_constrained = LaplacesDemon::invlogit(post2D + centring_term)) |>
    dplyr::relocate(post2D, .after = y_label)
}

#' Fit a Gaussian Process model
#' 
#' @param xygrid tibble with unobserved xy values (in case an exhaustive grid is not desired); if NULL, use `xvals` and `yvals` to create a grid of all possible combinations of the two coordinates
#' @inheritParams compute_posterior2D
fit_GP <- function(xvals, yvals, xygrid = NULL, xobs, yobs, zobs, k=ksqexp, l1=NA, b=1, meas_error=NA){
  # calculate covariances between unobserved and observed
  if(is.null(xygrid)) xygrid <- tidyr::expand_grid(x=xvals, y=yvals)
  xyobs <- tibble(x=xobs,y=yobs)
  
  kuo <- generate_2Dksqexp_covmat(xygrid,xyobs,fn=k,l=l1,b=b)  # 'relationship' pairwise between unobserved and observed
  kou <- generate_2Dksqexp_covmat(xyobs,xygrid,fn=k,l=l1,b=b)  # 'relationship' pairwise between observed and unobserved
  koo <- generate_2Dksqexp_covmat(xyobs,xyobs,fn=k,l=l1,b=b) + 1e-6*diag(nrow(xyobs))  # protect against non-invertibleness
  kuu <- generate_2Dksqexp_covmat(xygrid,xygrid,fn=k,l=l1,b=b)
  
  # calculate posterior mean and posterior cov matrix
  centring_term <- mean(LaplacesDemon::logit(zobs))
  logit_zobs_centred <- LaplacesDemon::logit(zobs) - centring_term  # use the logistic-transformed data centred around mean 0
  if (is.na(meas_error)) {
    post_mean <- kuo%*%solve(koo)%*%(logit_zobs_centred)  # conditional mean
    post_covmat <- kuu - (kuo%*%solve(koo)%*%kou)  # conditional variance
  } else if (!is.na(meas_error)) {
    errmat <- diag((meas_error^2), nrow(koo))
    post_mean <- kuo%*%solve(koo + errmat)%*%(logit_zobs_centred)  # conditional mean
    post_covmat <- kuu - (kuo%*%solve(koo + errmat)%*%kou)  # conditional variance
  }

  list(
    post_mean = post_mean,
    post_covmat = post_covmat
  )
}

#' Fit a Gaussian Process model
#' 
#' Leverage Cholesky decomposition for efficient computation
#' 
#' @param xygrid tibble with unobserved xy values (in case an exhaustive grid is not desired); if NULL, use `xvals` and `yvals` to create a grid of all possible combinations of the two coordinates
#' @inheritParams compute_posterior2D
fit_GP2 <- function(xvals, yvals, xygrid = NULL, xobs, yobs, zobs, k=ksqexp, l1=NA, b=1, meas_error=NA){
  # calculate covariances between unobserved and observed
  if(is.null(xygrid)) xygrid <- tidyr::expand_grid(x=xvals, y=yvals)
  xyobs <- tibble(x=xobs,y=yobs)
  
  kuo <- generate_2Dksqexp_covmat(xygrid,xyobs,fn=k,l=l1,b=b)  # 'relationship' pairwise between unobserved and observed
  kou <- generate_2Dksqexp_covmat(xyobs,xygrid,fn=k,l=l1,b=b)  # 'relationship' pairwise between observed and unobserved
  koo <- generate_2Dksqexp_covmat(xyobs,xyobs,fn=k,l=l1,b=b)
  kuu <- generate_2Dksqexp_covmat(xygrid,xygrid,fn=k,l=l1,b=b)

  # calculate posterior mean and posterior cov matrix
  centring_term <- mean(LaplacesDemon::logit(zobs))
  logit_zobs_centred <- LaplacesDemon::logit(zobs) - centring_term  # use the logistic-transformed data centred around mean 0

  if (is.na(meas_error)) meas_error <- 0
  errmat <- diag((meas_error^2), nrow(koo))
  post_mean <- kuo%*%chol2inv(chol(koo + errmat))%*%(logit_zobs_centred)  # conditional mean
  post_covmat <- kuu - (kuo%*%chol2inv(chol(koo + errmat))%*%kou)  # conditional variance

  list(
    post_mean = post_mean,
    post_covmat = post_covmat,
    centering_term = centring_term
  )
}