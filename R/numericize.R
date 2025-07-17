#' Parse character variable with numeric info into a numeric variable
#'
#' Fill in missing value for plotting
#' 
#' @param df data frame
#' @param col column to transform, as a symbol
numericize <- function(df, col){
  df |>
    rowwise() |> 
    mutate(
      {{col}} := list(pull_first({{col}}):pull_last({{col}}))
    ) |>
    unnest_longer({{col}})
}