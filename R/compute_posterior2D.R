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
compute_posterior2D <- function(xvals, yvals, xobs, yobs, zobs, k=ksqexp, l1=NA, ndrws=50, prov_levels) {
  #' calculate covariances between unobserved and observed
  xygrid <- expand_grid(x=xvals, y=yvals)
  xyobs <- tibble(x=xobs,y=yobs)
  
  kuo <- generate_2Dksqexp_covmat(xygrid,xyobs,fn=k,l=l1)  # 'relationship' pairwise between unobserved and observed
  kou <- generate_2Dksqexp_covmat(xyobs,xygrid,fn=k,l=l1)  # 'relationship' pairwise between observed and unobserved
  koo <- generate_2Dksqexp_covmat(xyobs,xyobs,fn=k,l=l1) + 1e-6*diag(nrow(xyobs))  # protect against non-invertibleness
  kuu <- generate_2Dksqexp_covmat(xygrid,xygrid,fn=k,l=l1)
  
  #' calculate posterior mean and posterior cov matrix
  post_mean <- kuo%*%solve(koo)%*%(logit(zobs))  # conditional mean
  post_covmat <- kuu - (kuo%*%solve(koo)%*%kou)  # conditional variance
  
  #' draw from the posterior distribution and record in dataframe post2D
  post2D <- tibble()
  for(i in 1:ndrws) {  # take 50 posterior draws
    post2D <- bind_rows(post2D, tibble(draw=i, x_i=xygrid$x, y_i=xygrid$y, post2D=rmnorm(1, post_mean, post_covmat + 1e-6*diag(nrow(post_covmat)))))  # each rmnorm is a random draw from the (multivariate) posterior
  }
  post2D <- post2D %>% mutate(post2D_constrained=invlogit(post2D)) %>%  # add column to transform back to [0,1]
    mutate(y_label = names(yvals)[match(y_i, yvals)]) %>%  # add column to return province names
    mutate(y_label = factor(y_label, levels=prov_levels)) %>%  # order provinces
    select(draw, x_i, y_i, y_label, post2D, post2D_constrained)  # reorder columns
  
  return(post2D)
}