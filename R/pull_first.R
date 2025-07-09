#' Pull first age out of string
#' 
#' @param x string
pull_first <- function(x){
  as.numeric(str_extract(x, "^[[:digit:]\\.]+"))
}