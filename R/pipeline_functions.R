attach_country <- function(df, country) {
  if ("pt" %in% names(df)) df <- df |> select(-pt)
  df |>
    mutate(country = !!country) |>
    relocate(country, .before = location)
}

prep_vax_data <- function(file_canada, file_england) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))

  vax_clean <- readr::read_csv(
    file_canada,
    show_col_types = FALSE
  ) |>
    filter(!(pt %in% c("SK", "YT", "NB")))

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
  ) |>
    filter(n_doses == "1+") |>
    # filter to common ages
    group_by(country) |>
    mutate(age_min = min(age_current), age_max = max(age_current)) |>
    ungroup() |>
    mutate(age_min = max(age_min), age_max = min(age_max)) |>
    filter(between(age_current, age_min, age_max))
}

get_sample_sizes <- function(vax) {
  vax |>
    filter(country == "Canada") |>
    group_by(location) |>
    summarize(n = n(), .groups = "drop")
}

split_train_test <- function(vax, sample_size, rep_id) {
  set.seed(rep_id) # reproducible results

  vax_england <- filter(vax, country == "England")

  sample_by_location <- tibble(
    location = sample(
      unique(vax_england$location),
      size = nrow(sample_size),
      replace = FALSE
    ),
    n = sample(sample_size$n)
  )

  vax_england_subset <- vax_england |>
    right_join(sample_by_location, by = "location")

  vax_train <- vax_england_subset |>
    group_split(location) |>
    purrr::map(\(df) slice_sample(df, n = df$n[1]) |> select(-n)) |>
    purrr::list_rbind()

  vax_test <- anti_join(vax_england_subset, vax_train)

  list(train = vax_train, test = vax_test, rep_id = rep_id)
}

fit_and_score <- function(split, prov_values = "UK-Gini") {
  scores <- select_GP(
    split$train,
    prov_values = prov_values,
    k_list = list(ksqexp = ksqexp, kexp = kexp),
    l1 = c(1, 1.25, 1.5, 1.75, 2, 2.25, 2.5),
    l2 = c(0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 6, 8, 10, 25, 50, 75, 100, 150, 200, 250, 500),
    b = c(0.5, 1, 1.5),
    meas_error = c(0.5, 0.1, 0.05, 0.01, 0)
  )

  opt_pars <- scores |> arrange(desc(lppd_exact)) |> slice(1)

  list(
    scores = scores,
    opt_pars = opt_pars,
    train = split$train,
    test = split$test,
    rep_id = split$rep_id
  )
}

evaluate_accuracy <- function(fit, prov_relation, error_tolerance) {
  opt_pars <- fit$opt_pars

  acc <- compute_accuracy(
    train = fit$train,
    test = fit$test,
    error_tolerance = error_tolerance,
    prov_values = prov_relation,
    k = get(opt_pars$k_name),
    l1 = opt_pars$l1,
    l2 = opt_pars$l2,
    b = opt_pars$b,
    meas_error = opt_pars$meas_error
  )

  tibble(
    rep_id = fit$rep_id,
    test_idx = seq_along(acc),
    accuracy = acc,
    true_value = fit$test$value
  )
}