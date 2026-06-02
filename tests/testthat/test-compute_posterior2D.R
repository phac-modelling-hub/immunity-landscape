test_that("original and Cholesky implementations of GP fitting are close enough", {
  # hyperparameters
  k <- ksqexp
  l1 <- 2.5
  l2 <- 2
  b <- 0.5
  meas_error <- 0.05

  # observations
  xobs <- c(10, 16, 14, 12, 17, 7, 9, 8, 6, 11, 9, 13, 6, 8, 16, 7, 11, 12, 14, 15, 11, 16, 8, 17, 13, 6, 14, 10, 12, 9, 15, 14, 7, 15, 9, 17, 7, 10, 13)
  yobs <- c(122.838, 122.838, 122.838, 122.838, 122.838, 122.838, 122.838, 122.838, 122.838, 122.838, 139.202, 139.202, 139.202, 139.202, 139.202, 139.202, 139.202, 139.202, 139.202, 139.202, 127.838, 127.838, 127.838, 127.838, 127.838, 127.838, 127.838, 127.838, 127.838, 127.838, 127.838, 123.01, 123.01, 117.962, 123.818, 123.818, 123.818, 123.818, 123.818)
  zobs <- c(0.9574, 0.9601, 0.9637, 0.9592, 0.9562, 0.9324, 0.9484, 0.9401, 0.931, 0.9592, 0.878, 0.9145, 0.844, 0.8663, 0.907, 0.8522, 0.8982, 0.9038, 0.9111, 0.9106, 0.9683, 0.9655, 0.9548, 0.9675, 0.9716, 0.954, 0.9755, 0.9701, 0.9663, 0.9635, 0.973, 0.9649, 0.93, 0.9597, 0.9472, 0.9595, 0.9321, 0.958, 0.9587)

  # test point
  x_a_x <- 7
  x_a_y <- 128

  fit_old <- fit_GP(
    # unobserved points
    xygrid = tibble::tibble(x = x_a_x, y = x_a_y*l2), 
    # observed points
    xobs = xobs,
    yobs = yobs*l2,
    zobs = zobs,
    # hyperparameters
    k = k, l1 = l1, b = b, meas_error = meas_error
  )

  fit_new <- fit_GP2(
    # unobserved points
    xygrid = tibble::tibble(x = x_a_x, y = x_a_y*l2), 
    # observed points
    xobs = xobs,
    yobs = yobs*l2,
    zobs = zobs,
    # hyperparameters
    k = k, l1 = l1, b = b, meas_error = meas_error
  )

  # check means
  expect_equal(fit_new$post_mean, fit_old$post_mean, tolerance = 1e-4) # won't be exactly equal because old method bumps covariance matrix of observed vs observed by 1e-6 to protect against non-invertibleness
  # check covariances
  expect_equal(fit_new$post_covmat, fit_old$post_covmat, tolerance = 1e-5) # don't need as low of a tolerance since (co)variances are squared
})
