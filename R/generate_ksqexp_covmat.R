#' Generate a squared exponential covariance matrix
#' 
#' @param x1 random vector 1 of model inputs x1_i (numeric vector)
#' @param x2 random vector 2 of model inputs x2_i (numeric vector)
#' @param fn covariance function of two parameters: 
#'    - abs(x1-x2) which is calculated piecewise in generate_ksqexp_covmat, 
#'    - lengthscale l. 
#'    fn is set to ksqexp by default; also works for kexp.
#' @param l lengthscale
#' @param b optional function scale determining the output variance for fn
generate_ksqexp_covmat <- function(x1,x2,fn=ksqexp,l,b=1) {
  expand_grid(x1=x1,x2=x2) %>% mutate(k=fn(abs(x1-x2),l,b)) %>%  # pairwise evaluation of cov function
    pull(k) %>% matrix(nrow=length(x1),byrow=T)
}