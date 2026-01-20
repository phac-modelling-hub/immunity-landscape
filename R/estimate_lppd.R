#' ESTIMATE the lppd for one GP model (i.e. for one choice of parameter values of {k, l1, l2, b}).
#' Note: relation of provinces currently uses GDP data.
#' 
#' @param vax_dataset data set of observed vaccine coverage data in our standardised format (tibble)
#' @param last_agecurrent last age for the GP model, i.e. largest x-variable entry (numeric)
#' @param prov_values named ordered vector giving numerical y-axis values associated to each province, unscaled
#' OR a character from the following list:
#'          - "GDP"
#'          - "low_income_families"
#'          - "vaccine_hesitancy".
#' @param ndrws specify number of draws to be taken from the prior and posterior distributions
#' @param k covariance function chosen from the following list: (function) **Functionality not added yet for matern.
#'          - ksqexp,
#'          - kexp,
#'          - kmatern.
#' @param l1 x-lengthscale parameter, for use with covariance functions ksqexp and/or kexp
#' @param l2 relative lengthscale of x values to y values, for use with covariance functions ksqexp and kexp
#' @param b scale for covariance function determining the output variance
#' @param meas_error if specified, measurement error is included (numeric)
estimate_lppd <- function(vax_dataset, last_agecurrent=NA, prov_values, ndrws=100, k=ksqexp, l1=NA, l2=NA, b=1, meas_error=NA) {
  # prepare the multiple training datasets (take-one-out)
  vax_dataset <- vax_dataset %>% filter((age_current<=last_agecurrent) & n_doses!="2")  # filter out unused data (2-dose & older ages)
  vax_datasets <- map(1:nrow(vax_dataset), ~ vax_dataset[-.x, ])
  names(vax_datasets) <- paste0("vax_dataset", 1:nrow(vax_dataset))
  
  # run model for each training set and extract 100 draw estimates at the removed (test) point
  lppd_value <- rep(NA,nrow(vax_dataset))
  for (i in 1:nrow(vax_dataset)) {
    xout <- vax_dataset[i, ] %>% pull(age_current)  # age_current of removed data point
    yout <- vax_dataset[i, ] %>% pull(location)  # province of removed data point
    zout <- vax_dataset[i, ] %>% pull(value)  # correct coverage value of removed data point
    ppd <- run_GP(vax_datasets[[i]], show_plots=F, last_agecurrent=last_agecurrent, prov_values=prov_values, ndrws=ndrws, k=k, l1=l1, l2=l2, b=b, meas_error=meas_error) %>%
      filter(x_i==xout & y_label==yout) %>% pull(post2D_constrained) %>% 
      between(zout - 0.02, zout + 0.02) %>% sum()  # freq of ppd evaluated at true removed value +/-2%
    lppd_value[i] <- ppd  # not taking logs currently; should be log(ppd) eventually
  }
  
  # sum lppd values from each test
  return(sum(lppd_value))
}