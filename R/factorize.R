#' Parse character variable with integer info into an ordered factor variable
#' 
#' @param df data frame
#' @param col column to transform, as a symbol
factorize <- function(df, col){
  df <- (df
         |> mutate(order = pull_numeric({{col}}))
         |> arrange(order)
         |> select(-order)
         |> mutate({{col}} := as.factor({{col}}))
  )
}