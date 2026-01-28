#' Compute the lppd analytically for one GP model (i.e. for one choice of parameter values of {k, l1, l2, b}) but from
#' leave-one-PROVINCE-out cross-validation.
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
compute_lppd_lopo <- function(vax_dataset, last_agecurrent=21, prov_values, k=ksqexp, l1=NA, l2=NA, b=1, meas_error=NA) {
  # prepare observed data (training+test)
  vax_dataset <- vax_dataset %>% filter((age_current<=last_agecurrent) & n_doses!="2")  # filter out unused data (2-dose & older ages)
  xobs <- vax_dataset %>% pull(age_current)
  prov_values <- extract_province_relation(prov_values, vax_dataset=vax_dataset)
  prov_values <- prov_values*l2  # scale province values by l2
  yobs <- prov_values[vax_dataset %>% pull(location)]  # this includes scaling by l2
  zobs <- vax_dataset %>% pull(value)
  centring_term <- mean(logit(zobs))
  logit_zobs_centred <- logit(zobs) - centring_term  # apply logistic transform and centre the data around mean 0
  
  # calculate koo, the covariance matrix of training+test points
  xyobs <- tibble(x=xobs,y=yobs)
  koo <- generate_2Dksqexp_covmat(xyobs,xyobs,fn=k,l=l1,b=b) + 1e-12*diag(nrow(xyobs))  # protect against non-invertibleness
  
  # calculate mean and variance of the ppd at each test point a
  if (is.na(meas_error)) {
    Kinv <- solve(koo)
  } else if (!is.na(meas_error)) {
    errmat <- diag((meas_error^2), nrow(koo))
    Kinv <- solve(koo + errmat)
  }
  Kinvz <- Kinv%*%(logit_zobs_centred)
  # province filtering -- *needs checking*
  provinces <- unique(vax_dataset$location)
  n_obs_per_province <- table(vax_dataset$location)
  lppd_by_province   <- setNames(numeric(length(provinces)), provinces)
  mean_lppd_by_province <- setNames(numeric(length(provinces)), provinces)
  lppd_total <- 0
  for (p in provinces) {
    idx <- which(vax_dataset$location == p)
    n_p <- length(idx)
    # block corresponding to held-out province
    Kinv_AA <- Kinv[idx, idx, drop = FALSE]
    Kinvz_A <- Kinvz[idx, , drop = FALSE]
    # conditional mean and covariance
    Sigma_A <- solve(Kinv_AA)
    mu_A <- logit_zobs_centred[idx] - Sigma_A %*% Kinvz_A
    # observed values for this province
    z_A <- logit_zobs_centred[idx]
    # multivariate normal log density
    lppd_p <- -0.5*(length(idx)*log(2*pi) + determinant(Sigma_A, logarithm = TRUE)$modulus + t(z_A - mu_A) %*% solve(Sigma_A) %*% (z_A - mu_A))
    lppd_p <- as.numeric(lppd_p)
    lppd_by_province[p] <- lppd_p
    mean_lppd_by_province[p] <- lppd_p / n_p
    lppd_total <- lppd_total + lppd_p
  }
  
  # for (i in 1:nrow(vax_dataset)) {
  #   zout <- vax_dataset[i, ] %>% pull(value)  # correct coverage value of removed data point
  #   centring_term_i <- if (length(centring_term) == 1) centring_term else centring_term[i]  # for centring the removed data point
  #   logit_zout_centred <- logit(zout) - centring_term_i
  #   ppd_means[i] <- logit_zout_centred - (Kinvz[i] / Kinv[i,i])
  #   ppd_vars[i] <- 1 / (Kinv[i,i])
  #   lppds[i] <- -(0.5*log(ppd_vars[i])) - (((logit_zout_centred - ppd_means[i])^2)/(2*ppd_vars[i])) - (0.5*log(2*pi))
  # }
  
  # sum lppd values from each test
  return(list(lppd = lppd_total, lppd_by_province = lppd_by_province,
              mean_lppd_by_province = mean_lppd_by_province, n_obs_per_province = n_obs_per_province))
}