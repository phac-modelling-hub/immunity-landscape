#' GP model selection -- identify optimal combination of k, l1, l2, b. Outputs a tibble detailing 
#' each set of model hyperparameters and a goodness-of-fit metric (sum(lppd)) using take-one-out cross validation. 
#' Large absolute lppd is optimal. The recommended parameter values are also printed to the console.
#' Note: covariance functions can be ksqexp and/or kexp.
#' 
#' @param vax_dataset data set of observed vaccine coverage data in our standardised format (tibble)
#' @param ndrws specify number of draws to be taken for estimate_lppd() (numeric)
#' @param prov_values any named ordered vector giving numerical y-axis values associated to each province, unscaled
#' OR a character from the following list (see extract_province_relation() for details):
#'          - "Gini" (default), for Gini index on adjusted household after-tax income from StatCan (mean of 2015 and 2020 values);
#'          - "GDP", for log(GDP) data from StatCan;
#'          - "low_income_families", for 2021 rate of children in low-income families (Market Basket Measure) from Health Inequalities Data Tool;
#'          - "vaccine_hesitancy", for 2017 prevalence of parents' vaccine hesitancy (refuse all + hesitant) from cNICS;
#'          - "UK-GDP", for log(2023 UK regional GDP) from ONS;
#'          - "UK-low_income_families", for FYE 2023 rate of children in low-income families (DWP Official Statistics);
#'          - "UK-Gini", for UK Gini index on total wealth (ONS, April 2016-March 2018).assumed to represent
#' current age, y province, and z vaccine coverage.
#' @param k_list a named list of covariance functions, e.g. list(ksqexp = ksqexp, kexp = kexp) (named list)
#' @param l1 a vector of x-lengthscale parameters for use with covariance functions ksqexp and/or kexp (vector)
#' @param l2 a vector of relative lengthscale parameters for use with covariance functions ksqexp and/or kexp (vector)
#' @param b a vector of scales for covariance function determining the output variances (vector)
#' @param meas_error if specified, measurement error is included (numeric)
select_GP <- function(vax_dataset, prov_values="Gini", ndrws=100, k_list=list(ksqexp=ksqexp), l1=NA, l2=NA, b=1, meas_error=NA) {
  # first separate k_list into functions and names
  k_tibble <- tibble(k_name = names(k_list), k = unname(k_list))
  
  # compute lppd for all combinations of parameters
  combinations <- tidyr::expand_grid(k_tibble, l1, l2, b, meas_error)  # functions need to be protected in a list
  combinations <- combinations %>% mutate(lppd_exact = purrr::pmap_dbl(
        list(k, l1, l2, b, meas_error),
        \(k_fn, l1_val, l2_val, b_val, me_val) compute_lppd(
          vax_dataset = vax_dataset,
          prov_values = prov_values,
          k = k_fn,
          l1 = l1_val,
          l2 = l2_val,
          b = b_val,
          meas_error = me_val
        )
      )) %>%
        select(-k) %>% mutate(model_no = row_number(), .before = 1)
  return(combinations)
}

