test_that("refactor of compute_lppd produces same values as before", {
  # choose inputs for compute_lppd()
  vax_clean <- readr::read_csv(here::here("data", "measles_vax-coverage-data-cleaned.csv"), show_col_types = FALSE) %>% filter(!(pt %in% c("SK","YT","NB")))
  prov_values <- "Gini"
  l1 <- 2.5
  l2 <- 10
  b <- 0.5
  meas_error <- 0.1

  expect_equal(
    # test code (refactored version)
    compute_lppd2(vax_clean, prov_values = prov_values, l1 = l1, l2 = l2, b = b, meas_error = meas_error),
    # expected outcome
    compute_lppd(vax_clean, prov_values = prov_values, l1 = l1, l2 = l2, b = b, meas_error = meas_error)
  )
})
