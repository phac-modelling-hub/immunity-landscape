#' Pull first age out of string
#' 
#' @param x string
pull_first <- function(x){
  as.numeric(stringr::str_extract(x, "^[[:digit:]\\.]+"))
}