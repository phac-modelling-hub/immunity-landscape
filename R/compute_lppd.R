#' COMPUTE the lppd analytically for one GP model (i.e. for one choice of parameter values of {k, l1, l2, b, meas_error}) from
#' leave-one-out cross-validation. 
#' See GitHub Issue #4 for details of method.
#' Note: scaling by l2 happens inside the function.
#' 
#' @param vax_dataset data set of observed vaccine coverage data in our standardised format (tibble)
#' @param last_agecurrent last age for the GP model, i.e. largest x-variable entry (numeric)
#' @param prov_values named ordered vector giving numerical y-axis values associated to each province, unscaled
#' OR a character from the following list:
#'          - "GDP"
#'          - "low_income_families"
#'          - "vaccine_hesitancy".
#' @param k covariance function chosen from the following list: (function) **Functionality not added yet for matern.
#'          - ksqexp,
#'          - kexp,
#'          - kmatern.
#' @param l1 x-lengthscale parameter, for use with covariance functions ksqexp and/or kexp
#' @param l2 relative lengthscale of x values to y values, for use with covariance functions ksqexp and kexp
#' @param b scale for covariance function determining the output variance
#' @param meas_error if specified, measurement error is included (numeric)
compute_lppd <- function(vax_dataset, last_agecurrent=21, prov_values, k=ksqexp, l1=NA, l2=NA, b=1, meas_error=NA) {
  # prepare observed data (training+test)
  vax_dataset <- vax_dataset %>% dplyr::filter((age_current<=last_agecurrent) & n_doses!="2+")  # filter out unused data (2-dose & older ages)
  xobs <- vax_dataset %>% dplyr::pull(age_current)
  prov_values <- extract_province_relation(prov_values, vax_dataset=vax_dataset)
  prov_values <- prov_values*l2  # scale province values by l2
  yobs <- prov_values[vax_dataset %>% dplyr::pull(location)]  # this includes scaling by l2
  zobs <- vax_dataset %>% dplyr::pull(value)
  centring_term <- mean(LaplacesDemon::logit(zobs))
  logit_zobs_centred <- LaplacesDemon::logit(zobs) - centring_term  # apply logistic transform and centre the data around mean 0
  
  # calculate koo, the covariance matrix of training+test points
  xyobs <- tibble::tibble(x=xobs,y=yobs)
  koo <- generate_2Dksqexp_covmat(xyobs,xyobs,fn=k,l=l1,b=b) + 1e-12*diag(nrow(xyobs))  # protect against non-invertibleness
  
  # calculate mean and variance of the ppd at each test point a
  if (is.na(meas_error)) {
    bottom <- solve(koo)
  } else if (!is.na(meas_error)) {
    errmat <- diag((meas_error^2), nrow(koo))
    bottom <- solve(koo + errmat)
  }
  top <- bottom%*%(logit_zobs_centred)
  ppd_means <- rep(NA,nrow(vax_dataset))
  ppd_vars <- rep(NA,nrow(vax_dataset))
  lppds <- rep(NA,nrow(vax_dataset))
  for (i in 1:nrow(vax_dataset)) {
    zout <- vax_dataset[i, ] %>% dplyr::pull(value)  # correct coverage value of removed data point
    centring_term_i <- if (length(centring_term) == 1) centring_term else centring_term[i]  # for centring the removed data point
    logit_zout_centred <- LaplacesDemon::logit(zout) - centring_term_i
    ppd_means[i] <- logit_zout_centred - (top[i] / bottom[i,i])
    ppd_vars[i] <- 1 / (bottom[i,i])
    lppds[i] <- -(0.5*log(ppd_vars[i])) - (((logit_zout_centred - ppd_means[i])^2)/(2*ppd_vars[i])) - (0.5*log(2*pi))
  }

  # sum lppd values from each test
  return(sum(lppds))
}

# Refactored to modularize (for accuracy calculation based on PPD means and variances)
# with help from Claude

#' Prepare inputs needed to compute posterior predictive density (PPD)
#' @inheritParams compute_lppd2
#' @return List containing `vax_dataset`, `bottom` (matrix for denominator of PPD mean equation), logit_zobs_centred (centred and transformed response values), and centring_term (mean of transformed response values used to centre them)
prepare_ppd_inputs <- function(vax_dataset, last_agecurrent = 21, prov_values, k = ksqexp, l1 = NA, l2 = NA, b = 1, meas_error = NA) {
vax_dataset <- vax_dataset %>% dplyr::filter((age_current <= last_agecurrent) & n_doses != "2+")
xobs <- vax_dataset %>% dplyr::pull(age_current)
prov_values <- extract_province_relation(prov_values, vax_dataset = vax_dataset)
prov_values <- prov_values * l2
yobs <- prov_values[vax_dataset %>% dplyr::pull(location)]
zobs <- vax_dataset %>% dplyr::pull(value)
centring_term <- mean(LaplacesDemon::logit(zobs))
logit_zobs_centred <- LaplacesDemon::logit(zobs) - centring_term

xyobs <- tibble::tibble(x = xobs, y = yobs)
koo <- generate_2Dksqexp_covmat(xyobs, xyobs, fn = k, l = l1, b = b) + 1e-12 * diag(nrow(xyobs))

bottom <- if (is.na(meas_error)) {
solve(koo)
} else {
solve(koo + diag(meas_error^2, nrow(koo)))
}
  
top <- bottom %*% logit_zobs_centred

list(
  vax_dataset = vax_dataset,
  bottom = bottom,
  top = top,
  logit_zobs_centred = logit_zobs_centred
)
}

#' Compute parameters for posterior predictive density (PPD)
#' @param prep Output from `prepare_ppd_inputs`
#' @return List containing `ppd_means` and `ppd_vars`
compute_ppd_params <- function(prep) {
bottom <- prep$bottom
top <- prep$top
n <- nrow(prep$vax_dataset)
zobs <- prep$vax_dataset %>% dplyr::pull(value)

# ppd_means <- rep(NA, n)
# ppd_vars <- rep(NA, n)
# for (i in seq_len(n)) {
ppd_means <- sapply(seq_len(n), \(i){prep$logit_zobs_centred[i] - (top[i] / bottom[i, i])})
ppd_vars <- sapply(seq_len(n), \(i){1 / bottom[i, i]})
# }

list(ppd_means = ppd_means, ppd_vars = ppd_vars)
}

#' COMPUTE the lppd analytically for one GP model (i.e. for one choice of parameter values of {k, l1, l2, b, meas_error}) from
#' leave-one-out cross-validation. 
#' See GitHub Issue #4 for details of method.
#' Note: scaling by l2 happens inside the function.
#' 
#' @param vax_dataset data set of observed vaccine coverage data in our standardised format (tibble)
#' @param last_agecurrent last age for the GP model, i.e. largest x-variable entry (numeric)
#' @param prov_values named ordered vector giving numerical y-axis values associated to each province, unscaled
#' OR a character from the following list:
#'          - "GDP"
#'          - "low_income_families"
#'          - "vaccine_hesitancy".
#' @param k covariance function chosen from the following list: (function) **Functionality not added yet for matern.
#'          - ksqexp,
#'          - kexp,
#'          - kmatern.
#' @param l1 x-lengthscale parameter, for use with covariance functions ksqexp and/or kexp
#' @param l2 relative lengthscale of x values to y values, for use with covariance functions ksqexp and kexp
#' @param b scale for covariance function determining the output variance
#' @param meas_error if specified, measurement error is included (numeric)
compute_lppd2 <- function(vax_dataset, last_agecurrent = 21, prov_values, k = ksqexp, l1 = NA, l2 = NA, b = 1, meas_error = NA) {
ppd_inputs <- prepare_ppd_inputs(vax_dataset, last_agecurrent, prov_values, k, l1, l2, b, meas_error)
ppd_params <- compute_ppd_params(ppd_inputs)

lppds <- -0.5 * log(ppd_params$ppd_vars) -
((ppd_inputs$logit_zobs_centred - ppd_params$ppd_means)^2 / (2 * ppd_params$ppd_vars)) -
0.5 * log(2 * pi)
sum(lppds)
}