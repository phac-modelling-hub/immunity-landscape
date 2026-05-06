#' Compute model accuracy against test points
#' 
#' What is the probability that the fitted model predicts each test (left out) point within a specified error tolerance?
#' 
#' @param train A data frame with the training points
#' @param test A data frame with the test points
#' @param error_tolerance A proportion specifying the acceptable error tolerance on the scale of vaccine coverage. For instance, error_tolerance=0.05 would mean the model's prediction of a vaccine coverage is considered accurate if it is within 5% of the test (true) value.
#' @inheritParams compute_lppd2
#' @return An accuracy (as a proportion/probability) for each test point
compute_accuracy <- function(train, test, error_tolerance = 0.05, last_agecurrent = 21, prov_values, k = ksqexp, l1 = NA, l2 = NA, b = 1, meas_error = NA) {
  ppd_inputs <- prepare_ppd_inputs(vax_dataset = dplyr::bind_rows(test, train), last_agecurrent = last_agecurrent, prov_values = prov_values, k = k, l1 = l1, l2 = l2, b = b, meas_error = meas_error)

  # compute parameters of the ppd at each test point (as a normal over logit-transformed and standardized vaccine coverage values)
  if (is.na(meas_error)) {
    Kinv <- solve(ppd_inputs$koo)
  } else if (!is.na(meas_error)) {
    errmat <- diag((meas_error^2), nrow(ppd_inputs$koo))
    Kinv <- solve(ppd_inputs$koo + errmat)
  }
  Kinvz <- Kinv%*%(ppd_inputs$logit_zobs_centred)
  idx <- 1:nrow(test)
  # block corresponding to test points
  Kinv_AA <- Kinv[idx, idx, drop = FALSE]
  Kinvz_A <- Kinvz[idx, , drop = FALSE]
  # observed values for test points
  z_A <- logit_zobs_centred[idx]
  # conditional mean and covariance
  ppd_var <- solve(Kinv_AA)
  ppd_mean <- z_A - ppd_var %*% Kinvz_A

  ppd_params <- list(
    ppd_means = ppd_mean,
    ppd_vars = diag(ppd_var) # pull diagonal elements
  )
 
  # create target interval
  # cap vaccine coverage between 0 and 1
  target_lwr <- sapply(test$value-error_tolerance/2, \(x) max(x, 0))
  target_upr <- sapply(test$value+error_tolerance/2, \(x) min(x, 1))

  # centre and standardized target interval
  target_lwr <- LaplacesDemon::logit(target_lwr) - ppd_inputs$centring_term
  target_upr <- LaplacesDemon::logit(target_upr) - ppd_inputs$centring_term
  
  # given a test obs (z value; vax coverage) and an acceptable level of error, how much probability mass is found in that interval?
  # evaluate the CDF at boundary of target interval and take the difference to get accuracy
  pnorm(target_upr, mean = ppd_params$ppd_means, sd = sqrt(ppd_params$ppd_vars)) - pnorm(target_lwr, mean = ppd_params$ppd_means, sd = sqrt(ppd_params$ppd_vars))
}