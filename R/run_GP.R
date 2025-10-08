#' Run the full 2D Gaussian Process model
#' 
#' First input `vax_dataset` must be specified. All other inputs are optional. Function will return a matrix of posterior draws.
#' Note: x and y are independent variables, with dependent variable z. x is assumed to represent
#' current age, y province, and z vaccine coverage.
#' @param vax_dataset data set of observed vaccine coverage data in our standardised format (tibble)
#' @param first_agecurrent first age for the GP model, i.e. smallest x-variable entry (numeric)
#' @param last_agecurrent last age for the GP model, i.e. largest x-variable entry (numeric)
#' @param prov_values any named ordered vector giving numerical y-axis values associated to each province, unscaled
#' OR a character from the following list:
#'          - "GDP"
#'          - ""
#' @param k covariance function chosen from the following list: (function)
#'          - ksqexp,
#'          - kexp.
#' @param k_name as above, but written as a character string for use in plot titles (character)
#' @param l1 x-lengthscale parameter, for use with covariance functions ksqexp and kexp
#' @param l2 relative lengthscale of x values to y values, for use with covariance functions ksqexp and kexp
#' @param b scale for covariance function determining the output variance
#' @param ndrws specify number of draws to be taken from the prior and posterior distributions
#' @param show_plots if false, plots are hidden
run_GP <- function(vax_dataset, first_agecurrent=1, last_agecurrent=32, prov_values=NA, k=ksqexp, k_name=NA, l1=NA, l2=NA, b=1, 
                   ndrws=50, show_plots=T) {
  #' define possible x values
  xvals <- first_agecurrent:last_agecurrent  # a vector of current ages
  
  #' define possible y values / province values
  if (all(is.na(prov_values))) {
    prov_values <- extract_province_relation("GDP", vax_dataset=vax_dataset) 
    print("yes")
  } else {
    prov_values <- extract_province_relation(prov_values, vax_dataset=vax_dataset)
  }
  yvals <- prov_values
  prov_levels <- names(prov_values)
  
  #' define covariance and related parameters
  if (is.na(k_name)) k_name <- "ksqexp"  # for plot titles
  if (is.na(l1)) l1 <- 1.5  # x lengthscale
  if (is.na(l2)) l2 <- 3  # relative lengthscale of x values to y values
  yvals <- yvals*l2  # scale y values to the x lengthscale
  
  #' compute and plot prior
  prior2D <- compute_prior2D(xvals=xvals, yvals=yvals, k=k, l1=l1, ndrws=ndrws, prov_levels=prov_levels, b=b)
  if (show_plots) prior2D %>% plot_prior2D(., k=k_name, k_param1=l1, k_param2=l2) %>% print()
  
  #' observe data (we may wish to use cNICS here instead of vax_clean)
  xobs <- vax_dataset %>% filter((age_current<=last_agecurrent) & n_doses!="2") %>% pull(age_current)
  yobs <- yvals[vax_dataset %>% filter((age_current<=last_agecurrent) & n_doses!="2") %>% pull(location)]  # this includes scaling by l2
  zobs <- vax_dataset %>% filter((age_current<=last_agecurrent) & n_doses!="2") %>% pull(value)
  logit_zobs_centred <- logit(zobs) - mean(logit(zobs))  # apply logistic transform and centre the data around mean 0
  if (show_plots) tibble(xobs, yobs, zobs, logit_zobs_centred) %>% print()
  
  #' compute and plot posterior
  post2D <- compute_posterior2D(xvals=xvals, yvals=yvals, xobs=xobs, yobs=yobs, zobs=zobs,
                                k=k, l1=l1, ndrws=ndrws, prov_levels=prov_levels, b=b)
  vax_clean <- readr::read_csv(here::here("data", "measles_vax-coverage-data-clean2.csv"), show_col_types = FALSE) %>%  #added for cNICS comparison
    filter(!(pt %in% c("SK","YT","NB")))
  if (show_plots) post2D %>% plot_posterior2D(., xobs=xobs, yobs=yobs, zobs=zobs, k=k_name, k_param1=l1, k_param2=l2, k_param3=b,
                                              vax_dataset=vax_clean, last_agecurrent=last_agecurrent, prov_levels=prov_levels) %>% print()
  return(post2D)
}