#' Compute model accuracy against test points
#' 
#' What is the probability that the fitted model predicts each test (left out) point within a specified error tolerance?
#' 
#' @param train A data frame with the training points with columns `age_current` (predictor 1, current age), `location` (predictor 2, categorical location label), and `value` (response, vaccine coverage estimate)
#' @param test A data frame with the training points with columns `age_current` (predictor 1, current age), `location` (predictor 2, categorical location label), and `value` (response, vaccine coverage estimate)
#' @param error_tolerance A proportion specifying the acceptable error tolerance on the scale of vaccine coverage. For instance, error_tolerance=0.05 would mean the model's prediction of a vaccine coverage is considered accurate if it is within 5% of the test (true) value.
#' @inheritParams compute_lppd2
#' @param prov_values String denoting which province-relation dataset to use in converting categorical location labels to numeric measure
#' @param eps A small positive number to ensure numerical stability (default is 1e-6)
#' @return An accuracy (as a proportion/probability) for each test point
compute_accuracy <- function(train, test, error_tolerance = 0.05, last_agecurrent = 21, prov_values = "GDP", k = ksqexp, l1 = NA, l2 = NA, b = 1, meas_error = NA, eps = 1e-6) {
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

  mu <-  fit$post_mean
  sigma2 <- diag(fit$post_covmat)
  
  # recycle scalars
  n <- nrow(test)
  error_tolerance <- rep(error_tolerance, length.out = n)
  meas_error      <- rep(meas_error,      length.out = n)

  # ---- compute tolerance bounds and transform ----
  lower_z <- pmax(test$value - error_tolerance/2, eps)
  upper_z <- pmin(test$value + error_tolerance/2, 1 - eps)
  # stabilize to (0, 1)
  lower_z <- pmin(pmax(lower_z, eps), 1 - eps)
  upper_z <- pmin(pmax(upper_z, eps), 1 - eps)

  invalid <- lower_z >= upper_z # indices for invalid intervals

  lower_w <- LaplacesDemon::logit(lower_z) - fit$centering_term
  upper_w <- LaplacesDemon::logit(upper_z) - fit$centering_term

  # ---- get total variance (model + measurement error) ----
  total_var <- sigma2 + meas_error
  total_sd  <- sqrt(total_var)

  # ---- compute accuracy probabilities ----
  A <- stats::pnorm(upper_w, mean = mu, sd = total_sd) - stats::pnorm(lower_w, mean = mu, sd = total_sd)
  A[invalid] <- NA
  A
}