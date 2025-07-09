#' Pull last age out of string
#' 
#' @param x string
pull_last <- function(x, last_age){
  if(str_detect(x, "\\+$")) return(last_age)
  as.numeric(str_extract(x, "[[:digit:]\\.]+$"))
}