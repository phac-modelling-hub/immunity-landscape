#' Compute a 2D Gaussian Process prior
#' 
#' This function computes the prior for the relationship between independent variables x,y and 
#' dependent variable z.
#' @param xvals numeric vector defining possible range of x-values
#' @param yvals named numeric vector defining possible range of y-values, pre-scaled by l2, the 
#'              relative lengthscale of x values to y values
#' @param k covariance function chosen from the following list:  **Functionality not added yet.
#'          - ksqexp, 
#'          - kexp, 
#'          - kmatern.
#' @param l1 optional x-lengthscale parameter, for use with covariance functions ksqexp and kexp
#' @param ndrws optional specify number of draws from the prior distribution
#' @param prov_levels factor defining the names of provinces
#' @param b optional function scale determining the output variance for k
compute_prior2D <- function(xvals, yvals, k=ksqexp, l1=NA, ndrws=50, prov_levels, b=1) {
  # covariance matrix in 2D
  npts <- length(xvals)*length(yvals)
  covmat <- expand_grid(x1=xvals, y1=yvals, x2=xvals, y2=yvals) %>% mutate(k=k(sqrt((x1-x2)^2 + (y1-y2)^2), l1, b)) %>%
    pull(k) %>% matrix(nrow=npts)
  
  # define prior and take 50 draws
  xygrid <- expand_grid(x=xvals, y=yvals)
  
  ## ORIGINAL WAY
  # prior2D <- tibble()
  # for(i in 1:ndrws){  # 50 realisations from a 2D gaussian process, with both x, y as independent variables
  #   prior2D <- bind_rows(prior2D, tibble(draw=i, x_i=xygrid$x, y_i=xygrid$y, prior2D=rmnorm(1, rep(0,npts), covmat + 1e-6*diag(npts))))
  # }
  # # add y-value names
  # prior2D <- prior2D %>% mutate(y_label = names(yvals)[match(y_i, yvals)]) %>% 
  #   mutate(y_label = factor(y_label, levels=prov_levels))
  
  ## NEW WAY 1
  # # perform all draws from prior from prior
  # prior2D <- lapply(1:ndrws, \(x) rmnorm(1, rep(0,npts), covmat + 1e-6*diag(npts)))
  # names(prior2D) <- paste0("prior_", 1:ndrws)
  # 
  # # attach to xygrid and massage into desired output format
  # prior2D <- dplyr::bind_cols(
  #   xygrid |> dplyr::rename(x_i = x, y_i = y),
  #   as_tibble(prior2D) # transpose for dims to match
  # ) |>
  #   # add y-value names
  #   dplyr::mutate(y_label = names(yvals)[match(y_i, yvals)]) |>
  #   dplyr::mutate(y_label = factor(y_label, levels = prov_levels)) |>
  #   # pivot to long format and arrange to match previous output
  #   tidyr::pivot_longer(
  #     tidyselect::starts_with("prior"),
  #     names_to = "draw",
  #     values_to = "prior2D", 
  #     names_transform = \(x) as.integer(gsub("prior_", "", x))
  #   ) |> 
  #   dplyr::arrange(draw) |>
  #   dplyr::relocate(draw) |>
  #   dplyr::relocate(y_label, .after = prior2D)
  
  ## NEW WAY 2
  prior2D <- rmnorm(n = ndrws, rep(0,npts), covmat + 1e-6*diag(npts)) # returns matrix with one row per draw
  rownames(prior2D) <- paste0("prior_", 1:ndrws)
  
  # attach to xygrid and massage into desired output format
  prior2D <- dplyr::bind_cols(
    xygrid |> dplyr::rename(x_i = x, y_i = y),
    as_tibble(prior2D |> t()) # transpose for dims to match
  ) |>
    # add y-value names
    dplyr::mutate(y_label = names(yvals)[match(y_i, yvals)]) |>
    dplyr::mutate(y_label = factor(y_label, levels = prov_levels)) |>
    # pivot to long format and arrange to match previous output
    tidyr::pivot_longer(
      tidyselect::starts_with("prior"),
      names_to = "draw",
      values_to = "prior2D", 
      names_transform = \(x) as.integer(gsub("prior_", "", x))
    ) |> 
    dplyr::arrange(draw) |>
    dplyr::relocate(draw) |>
    dplyr::relocate(y_label, .after = prior2D)
  
  return(prior2D)
}