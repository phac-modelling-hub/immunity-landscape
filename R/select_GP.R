#' GP model selection -- identify optimal combination of k, l1, l2, b. Outputs a tibble detailing 
#' each set of model hyperparameters and a goodness-of-fit metric (sum(lppd)) using take-one-out cross validation. 
#' Large absolute lppd is optimal. The recommended parameter values are also printed to the console.
#' Note: covariance functions can be ksqexp and/or kexp.
#' 
#' @param vax_dataset data set of observed vaccine coverage data in our standardised format (tibble)
#' @param last_agecurrent last age for the GP model, i.e. largest x-variable entry (numeric)
#' @param ndrws specify number of draws to be taken for estimate_lppd() (numeric)
#' @param prov_values named ordered vector giving numerical y-axis values associated to each province, unscaled
#' OR a character from the following list:
#'          - "GDP"
#'          - "low_income_families"
#'          - "vaccine_hesitancy".
#' @param k_list a named list of covariance functions, e.g. list(ksqexp = ksqexp, kexp = kexp) (named list)
#' @param l1 a vector of x-lengthscale parameters for use with covariance functions ksqexp and/or kexp (vector)
#' @param l2 a vector of relative lengthscale parameters for use with covariance functions ksqexp and/or kexp (vector)
#' @param b a vector of scales for covariance function determining the output variances (vector)
#' @param meas_error if specified, measurement error is included (numeric)
select_GP <- function(vax_dataset, last_agecurrent=21, prov_values, ndrws=100, k_list=list(ksqexp=ksqexp), l1=NA, l2=NA, b=1, meas_error=NA) {
  # first separate k_list into functions and names
  k_tibble <- tibble(k_name = names(k_list), k = unname(k_list))
  
  # compute lppd for all combinations of parameters
  combinations <- tidyr::expand_grid(k_tibble, l1, l2, b, meas_error)  # functions need to be protected in a list
  # combinations <- combinations %>% mutate(lppd_estimate = pmap_dbl(list(k, l1, l2, b), ~ estimate_lppd(  # method 1: estimate (l)ppd
  #   vax_dataset = vax_dataset,
  #   last_agecurrent = last_agecurrent,
  #   prov_values = prov_values,
  #   ndrws = ndrws,
  #   k = ..1,
  #   l1 = ..2,
  #   l2 = ..3,
  #   b = ..4)))
  combinations <- combinations %>% mutate(lppd_exact = purrr::pmap_dbl(list(k, l1, l2, b, meas_error), ~ compute_lppd2(  # method 2: compute lppd analytically
    vax_dataset = vax_dataset,
    last_agecurrent = last_agecurrent,
    prov_values = prov_values,
    k = ..1,
    l1 = ..2,
    l2 = ..3,
    b = ..4,
    meas_error = ..5))) %>% 
    mutate(lppd_LOPO = purrr::pmap_dbl(list(k, l1, l2, b, meas_error), ~ compute_lppd_lopo(  # compute lppd from leave-one-province-out CV
      vax_dataset = vax_dataset,
      last_agecurrent = last_agecurrent,
      prov_values = prov_values,
      k = ..1,
      l1 = ..2,
      l2 = ..3,
      b = ..4,
      meas_error = ..5)$lppd)) %>%
    select(-k) %>% mutate(model_no = row_number(), .before = 1)
  
  optimal_model <- combinations %>% slice_max(lppd_exact) %>% select(k_name, l1, l2, b)
  print(c("Optimal hyperparameters:", optimal_model))
  return(combinations)
}