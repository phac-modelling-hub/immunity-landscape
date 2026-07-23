#' Add full sequence of age groups, adding new limits
#' 
#' @param df a data frame where ages are stored in the `age_current` column
#' @param new_ages new ages to add before completing the sequence
#' @return a data frame
expand_age <- function(df, new_ages){
  df |>
  # attach new ages
  group_modify(\(x, ...) add_row(x, age_current = new_ages)) |>
  # fill in all missing ages from expanded range
  complete(age_current = full_seq(age_current, period = 1))
}

#' Parse age groups into one observation per single-year age
#'
#' @param df data frame
#' @param col column to transform, as a symbol
parse_age_groups <- function(df, col, last_age = 21){
  df |>
    rowwise() |> 
    mutate(
      {{col}} := list(pull_first({{col}}):pull_last({{col}}, last_age))
    ) |>
    unnest_longer({{col}})
}