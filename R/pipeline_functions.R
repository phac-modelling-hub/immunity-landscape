#' Attach a country label to a data frame
#'
#' Replaces any existing `pt` (province/territory) column with a `country`
#' column positioned before `location`.
#'
#' @param df A data frame containing at least a `location` column. If a `pt`
#'   column is present, it will be removed.
#' @param country A string giving the country name to attach (e.g. `"Canada"`,
#'   `"England"`). Injected via tidy evaluation (`!!`).
#'
#' @return A tibble with `country` as the first column (before `location`) and
#'   any former `pt` column removed.
attach_country <- function(df, country) {
  if ("pt" %in% names(df)) df <- df |> select(-pt)
  df |>
    mutate(country = !!country) |>
    relocate(country, .before = location)
}

#' Prepare combined Canada--England vaccination coverage data
#'
#' Reads and harmonises MMR1 vaccination coverage data from Canada
#' (province-level, cleaned) and England (region-level, UKHSA), then filters
#' to 1+ dose estimates and the overlapping age range between the two
#' countries.
#'
#' @param file_canada Path to the cleaned Canadian vaccination coverage CSV
#'   (columns: `pt`, `location`, `year_report`, `age`, `n_doses`, `value`,
#'   etc.). Provinces SK, YT, and NB are excluded due to data limitations.
#' @param file_england Path to the UKHSA MMR1 regional coverage CSV (columns:
#'   `geography`, `year`, `metric_value`, etc.). Coverage is recorded at age 5.
#'
#' @return A tibble with columns `country`, `location`, `year_report`,
#'   `year_birth`, `age`, `n_doses`, `value`, `age_current`, `age_min`, and
#'   `age_max`, filtered to the `"1+"` dose stratum and the age range common
#'   to both countries.
#'
#' @details
#' England records are assumed to represent 1-dose coverage at age 5.
#' `age_current` is computed as `current_year - year_report + age`, where
#' `current_year` is determined at runtime via [Sys.Date()]. The resulting
#' data is filtered to the intersection of age ranges present in both
#' countries.
prep_vax_data <- function(file_canada, file_england) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  
  vax_clean <- readr::read_csv(
    file_canada,
    show_col_types = FALSE
  ) |> dplyr::select(-source)

  vax_england <- readr::read_csv(
    file_england,
    show_col_types = FALSE
  ) |>
    mutate(
      age = 5, n_doses = "1+",
      value = metric_value / 100,
      location = geography,
      year_report = year,
      age_current = current_year - year_report + age
    ) |>
    select(location, year_report, age, n_doses, value, age_current)
    
  bind_rows(
    vax_clean |> attach_country("Canada"),
    vax_england |> attach_country("England")
  )
}

#' Compute per-location sample sizes from Canadian data
#'
#' Counts the number of observations per Canadian province/territory, which
#' is later used to determine how many observations to subsample from each
#' English region when constructing training sets.
#'
#' @param vax A tibble of vaccination coverage data as returned by
#'   [prep_vax_data()], containing at least `country` and `location` columns.
#'
#' @return A tibble with columns `location` and `n`, giving the number of
#'   rows per Canadian location.
get_sample_sizes <- function(vax) {
  age_range <- vax |>
    filter(country == "England") |>
    summarize(
      min = min(age_current, na.rm = TRUE),
      max = max(age_current, na.rm = TRUE)
    )

  vax |>
    filter(
      country == "Canada",
      between(age_current, age_range$min, age_range$max)
    ) |>
    group_by(location) |>
    summarize(n = n(), .groups = "drop")
}

#' Split England data into training and test sets
#'
#' Randomly selects a subset of English regions (matching the number of
#' Canadian provinces) and subsamples observations within each region
#' (matching the per-province sample sizes from Canada). The remaining
#' England observations form the test set. This mimics the structure of the
#' Canadian data for cross-validation purposes.
#'
#' @param vax A tibble of vaccination coverage data as returned by
#'   [prep_vax_data()], containing both Canadian and English observations.
#' @param sample_size A tibble with columns `location` and `n`, as returned
#'   by [get_sample_sizes()], defining the number of observations to draw per
#'   region.
#' @param id_rep Integer identifier for this repetition, used as the random
#'   seed via [set.seed()] for reproducibility.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{train}{A tibble of subsampled England observations forming the
#'       training set.}
#'     \item{test}{A tibble of remaining England observations forming the
#'       test set (i.e. observations not selected for training).}
#'     \item{id_rep}{The repetition identifier, passed through for
#'       downstream tracking.}
#'   }
split_train_test <- function(vax, sample_size, id_rep) {
  set.seed(id_rep) # reproducible results

  vax_england <- filter(vax, country == "England")

  sample_by_location <- tibble(
    location = unique(vax_england$location),
    n = sample(sample_size$n, size = length(unique(vax_england$location)), replace = FALSE)
  )

  vax_england_subset <- vax_england |>
    right_join(sample_by_location, by = "location")

  vax_train <- vax_england_subset |>
    group_split(location) |>
    purrr::map(\(df) slice_sample(df, n = unique(df$n)) |> select(-n)) |>
    purrr::list_rbind()

  vax_test <- anti_join(
    vax_england_subset, vax_train,
    by = join_by(
      country, location, year_report, year_birth, age,
      n_doses, value, age_current
    )
  )

  list(train = vax_train, test = vax_test, id_rep = id_rep)
}

#' Fit Gaussian Process hyperparameters on a training split
#'
#' Performs an exhaustive grid search over GP hyperparameters (kernel type,
#' lengthscales, output scale, measurement error) using [select_GP()], then
#' selects the combination with the highest exact log pointwise predictive
#' density (lppd).
#'
#' @param split A named list as returned by [split_train_test()], containing
#'   `train` (training data tibble), `test` (test data tibble), and `id_rep`
#'   (repetition identifier).
#' @param prov_relation A string specifying the socioeconomic dataset used to
#'   convert categorical location labels to a numeric axis in the GP model.
#'   Passed to [extract_province_relation()]. Options include `"UK-Gini"`,
#'   `"UK-GDP"`, `"UK-low_income_families"`, `"GDP"`, `"Gini"`,
#'   `"low_income_families"`, and `"vaccine_hesitancy"`. Default is
#'   `"UK-Gini"`.
#'
#' @return A named list with five elements:
#'   \describe{
#'     \item{id_rep}{Integer repetition identifier.}
#'     \item{data}{A list containing `train` and `test` tibbles.}
#'     \item{prov_relation}{The province relation string used.}
#'     \item{hyperparams}{A single-row tibble of the best hyperparameters
#'       (`model_no`, `k_name`, `l1`, `l2`, `b`, `meas_error`, `lppd_exact`,
#'       `lppd_LOPO`).}
#'     \item{score_hyperparams}{A tibble of all evaluated hyperparameter
#'       combinations with their lppd scores, for diagnostics.}
#'   }
#'
#' @details
#' The hyperparameter grid searched is:
#' \itemize{
#'   \item Kernels: [ksqexp()], [kexp()]
#'   \item `l1` (age lengthscale): 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5
#'   \item `l2` (location lengthscale): 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3,
#'     3.5, 4, 6, 8, 10, 25, 50, 75, 100, 150, 200, 250, 500
#'   \item `b` (output scale): 0.5, 1, 1.5
#'   \item `meas_error`: 0.5, 0.1, 0.05, 0.01, 0
#' }
fit_hyperparams <- function(split, prov_relation = "UK-Gini") {
  score_hyperparams <- select_GP(
    split$train,
    prov_values = prov_relation,
    k_list = list(ksqexp = ksqexp, kexp = kexp),
    l1 = c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5),
    l2 = c(0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 6, 8, 10, 25, 50, 75, 100, 150, 200, 250, 500),
    b = c(0.5, 1, 1.5),
    meas_error = c(0.5, 0.1, 0.05, 0.01, 0)
  )

  hyperparams <- score_hyperparams |> arrange(desc(lppd_exact)) |> slice(1)

  list(
    id_rep = split$id_rep,
    data = list(
      train = split$train,
      test = split$test
    ),
    prov_relation = prov_relation,
    hyperparams = hyperparams,
    score_hyperparams = score_hyperparams # just for diagnostics
  )
}

#' Compute model accuracy against held-out test points
#'
#' Evaluates the fitted GP model's predictive accuracy by computing, for each
#' test observation, the probability that the model's predictive distribution
#' places within a specified error tolerance of the true vaccine coverage
#' value.
#'
#' @param fit_hp A named list as returned by [fit_hyperparams()], containing:
#'   \describe{
#'     \item{id_rep}{Integer repetition identifier.}
#'     \item{data}{A list with `train` and `test` tibbles, each containing
#'       columns `age_current` (predictor 1), `location` (predictor 2,
#'       categorical), and `value` (response, vaccine coverage proportion).}
#'     \item{prov_relation}{String specifying the province-relation dataset
#'       used by [extract_province_relation()] to convert categorical
#'       locations to numeric values.}
#'     \item{hyperparams}{Single-row tibble of best-fit hyperparameters
#'       (`k_name`, `l1`, `l2`, `b`, `meas_error`).}
#'   }
#' @param error_tolerance A numeric proportion specifying the acceptable
#'   two-sided error tolerance on the vaccine coverage scale. For example,
#'   `0.05` means a prediction is "accurate" if it falls within \eqn{\pm 2.5}
#'   percentage points of the true value. Default is `0.05`.
#' @param last_agecurrent Integer giving the maximum current age considered
#'   in the GP model (not currently used in the function body but retained
#'   for interface consistency). Default is `21`.
#' @param eps A small positive number added for numerical stability when
#'   bounding logit-transformed tolerance intervals away from 0 and 1.
#'   Default is `1e-6`.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{id_rep}{Integer repetition identifier.}
#'     \item{fit_data}{The output of [fit_GP2()], containing `post_mean`
#'       (posterior mean vector), `post_covmat` (posterior covariance matrix),
#'       and `centering_term` (mean of logit-transformed observed values).}
#'     \item{score_accuracy}{A numeric vector of length `nrow(test)`, where
#'       each element is the probability that the GP predictive distribution
#'       falls within the tolerance band around the corresponding test
#'       observation. `NA` for observations where the tolerance interval is
#'       degenerate.}
#'   }
#'
#' @details
#' The accuracy probability for each test point is computed as
#' \deqn{A_i = \Phi\!\left(\frac{w_i^+ - \mu_i}{\sigma_i}\right) -
#'   \Phi\!\left(\frac{w_i^- - \mu_i}{\sigma_i}\right)}
#' where \eqn{w_i^{\pm}} are the logit-transformed and centred tolerance
#' bounds, \eqn{\mu_i} is the posterior mean, and
#' \eqn{\sigma_i = \sqrt{\sigma^2_i + \sigma^2_{\text{meas}}}} is the total
#' standard deviation combining posterior variance and measurement error.
compute_accuracy <- function(fit_hp, error_tolerance = 0.05, last_agecurrent = 21, eps = 1e-6) {
  # unpack hyperparameter fit object
  id_rep <- fit_hp$id_rep
  prov_relation <- fit_hp$prov_relation
  test <- fit_hp$data$test
  train <- fit_hp$data$train
  hyperparams <- fit_hp$hyperparams

  # transform categorical y variable into numeric and incorporate l2 scaling
  all <- dplyr::bind_rows(test, train)
  prov_values_lookup <- extract_province_relation(prov_relation, vax_dataset = all) * hyperparams$l2
  yunobs <- prov_values_lookup[all$location[1:nrow(test)]]
  yobs <- prov_values_lookup[all$location[(nrow(test) + 1):nrow(all)]]

  fit_data <- fit_GP2(
    # unobserved points
    xygrid = tibble::tibble(x = test$age_current, y = yunobs),
    # observed points
    xobs = train$age_current,
    yobs = yobs,
    zobs = train$value,
    # fit parameters
    k = get(hyperparams$k_name),
    l1 = hyperparams$l1,
    b = hyperparams$b,
    meas_error = hyperparams$meas_error
  )

  mu <- fit_data$post_mean
  sigma2 <- diag(fit_data$post_covmat)

  # recycle scalars
  n <- nrow(test)
  error_tolerance <- rep(error_tolerance, length.out = n)
  meas_error <- rep(hyperparams$meas_error, length.out = n)

  # ---- compute tolerance bounds and transform ----
  lower_z <- pmax(test$value - error_tolerance / 2, eps)
  upper_z <- pmin(test$value + error_tolerance / 2, 1 - eps)
  # stabilize to (0, 1)
  lower_z <- pmin(pmax(lower_z, eps), 1 - eps)
  upper_z <- pmin(pmax(upper_z, eps), 1 - eps)

  invalid <- lower_z >= upper_z # indices for invalid intervals

  lower_w <- LaplacesDemon::logit(lower_z) - fit_data$centering_term
  upper_w <- LaplacesDemon::logit(upper_z) - fit_data$centering_term

  # ---- get total variance (model + measurement error) ----
  total_var <- sigma2 + meas_error
  total_sd <- sqrt(total_var)

  # ---- compute accuracy probabilities ----
  A <- stats::pnorm(upper_w, mean = mu, sd = total_sd) -
    stats::pnorm(lower_w, mean = mu, sd = total_sd)
  A[invalid] <- NA
  
  # ---- output ----
  list(
    id_rep = id_rep,
    fit_data = fit_data,
    score_accuracy = A
  )
}