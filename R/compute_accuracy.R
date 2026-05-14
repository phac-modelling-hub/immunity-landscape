#' Compute model accuracy against test points
#' 
#' What is the probability that the fitted model predicts each test (left out) point within a specified error tolerance?
#' 
#' @param train A data frame with the training points with columns `age_current` (predictor 1, current age), `location` (predictor 2, categorical location label), and `value` (response, vaccine coverage estimate)
#' @param test A data frame with the training points with columns `age_current` (predictor 1, current age), `location` (predictor 2, categorical location label), and `value` (response, vaccine coverage estimate)
#' @param error_tolerance A proportion specifying the acceptable error tolerance on the scale of vaccine coverage. For instance, error_tolerance=0.05 would mean the model's prediction of a vaccine coverage is considered accurate if it is within 5% of the test (true) value.
#' @inheritParams compute_lppd2
#' @param prov_values String denoting which province-relation dataset to use in converting categorical location labels to numeric measure
#' @return An accuracy (as a proportion/probability) for each test point
compute_accuracy <- function(train, test, error_tolerance = 0.05, last_agecurrent = 21, prov_values = "GDP", k = ksqexp, l1 = NA, l2 = NA, b = 1, meas_error = NA) {
  # transform categorical y variable into numeric and incorporate l2 scaling
  all <- dplyr::bind_rows(test, train)
  prov_values <- extract_province_relation(prov_values, vax_dataset=all)*l2
  yunobs <- prov_values[all$location[1:nrow(test)]]
  yobs <- prov_values[all$location[(nrow(test)+1):nrow(all)]]

  fit <- fit_GP2(
    # unobserved points
    xygrid = tibble::tibble(x = test$age_current, y = yunobs), 
    # observed points
    xobs = train$age_current,
    yobs = yobs,
    zobs = train$value,
    k = k, l1 = l1, b = b, meas_error = meas_error)

  ppd_params <- list(
    ppd_means = fit$post_mean,
    ppd_vars = diag(fit$post_covmat) # pull diagonal elements
  )

  return(ppd_params)
 
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