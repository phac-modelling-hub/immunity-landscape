#' Pull last age out of string
#' 
#' @param x string
pull_last <- function(x, last_age){
  if(stringr::str_detect(x, "\\+$")) return(last_age)
  as.numeric(stringr::str_extract(x, "[[:digit:]\\.]+$"))
}