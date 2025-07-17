#' Pull first number from string
#' 
#' @param x string
pull_numeric <- function(x){
  as.numeric(str_extract(x, "[[:digit:]]+"))
}