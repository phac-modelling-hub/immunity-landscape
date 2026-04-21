#' Generate a 2D squared exponential covariance matrix
#' 
#' This function calculates the covariances in systems with two independent variables x and y.
#' @param xy1 random matrix 1 of model input pairs x, y (numeric vector)
#' @param xy2 random matrix 2 of model input pairs x, y (numeric vector)
#' @param fn covariance function of two parameters: 
#'    - sqrt((x1-x2)^2 + (y1-y2)^2) which is calculated piecewise in generate_2Dksqexp_covmat, 
#'    - lengthscale l. 
#'    fn is set to ksqexp by default; also works for kexp.
#' @param l lengthscale
#' @param b optional function scale determining the output variance for fn
#' 
#' Note that l is the lengthscale of the x variable. If the y's have a different lengthscale,
#' it is advisable to first scale y1 and y2 before inputting into the function.

generate_2Dksqexp_covmat <- function(xy1,xy2,fn=ksqexp,l,b=1) {
   xy1 <- xy1 %>% mutate(i = row_number())  # add index col to enable joining
   xy2 <- xy2 %>% mutate(j = row_number())  # add index col to enable joining
   
   tidyr::expand_grid(i = xy1$i, j = xy2$j) %>%  # slightly faster version of expand_grid
    left_join(xy1, by="i") %>%
    left_join(xy2, by="j", suffix = c("1", "2")) %>%  # suffix avoids duplicate column names
    mutate(k=fn(sqrt((x1 - x2)^2 + (y1 - y2)^2),l,b)) %>%  # pairwise evaluation of cov function
    pull(k) %>% matrix(nrow=nrow(xy1),byrow=T)
}