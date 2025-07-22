#' Generate a squared exponential covariance matrix
#' 
#' @param y1 random vector 1, with elements/ model parameters y1_i (numeric vector)
#' @param y2 random vector 2, with elements/ model parameters y2_i (numeric vector)
#' @param fn covariance function of two parameters: 
#'    - abs(y1-y2) which is calculated piecewise in generate_ksqexp_covmat, 
#'    - lengthscale l. 
#'    fn is set to ksqexp by default; also works for kexp.
#' @param l lengthscale

generate_ksqexp_covmat <- function(y1,y2,fn=ksqexp,l) {
  expand_grid(y1=y1,y2=y2) %>% mutate(k=fn(abs(y1-y2),l)) %>%  # pairwise evaluation of cov function
    pull(k) %>% matrix(nrow=length(y1),byrow=T)
}
