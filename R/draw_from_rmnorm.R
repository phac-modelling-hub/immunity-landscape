#' Wrapper around rmnorm() that returns prior/posterior draws in a standard format
#' 
#' @param n number of draws
#' @param mean vector of means
#' @param varcov variance-covariance matrix of the distribution
#' @inheritParams compute_prior2D
#' 
#' @seealso [mnormt::rmnorm()]
draw_from_rmnorm <- function(n, mean, varcov, xvals, yvals, prov_levels){
  
  # perform draws 
  draws <- mnormt::rmnorm(n, mean, varcov)
  rownames(draws) <- paste0("draw_", 1:n)
  
  # attach to xygrid and massage into desired output format
  draws <- dplyr::bind_cols(
    tidyr::expand_grid(x_i=xvals, y_i=yvals), # xygrid
    tibble::as_tibble(draws |> t()) # transpose for dims to match
  ) |>
    # add y-value names
    dplyr::mutate(y_label = names(yvals)[match(y_i, yvals)]) |>
    dplyr::mutate(y_label = factor(y_label, levels = prov_levels)) |>
    # pivot to long format and arrange to match previous output
    tidyr::pivot_longer(
      tidyselect::starts_with("draw"),
      names_to = "draw",
      names_transform = \(x) as.integer(gsub("draw_", "", x))
    ) |> 
    dplyr::arrange(draw) |>
    dplyr::relocate(draw) |>
    dplyr::relocate(y_label, .after = value)
}