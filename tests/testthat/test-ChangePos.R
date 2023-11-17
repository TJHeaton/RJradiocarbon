test_that("ChangePos gives expected outcomes", {

  set.seed(14)
  rate_s <- initial_rate_s <- c(0, 2, 5, 7, 10)
  rate_h <- initial_rate_h <- c(5, 2, 4, 6)
  n_changepoints <- length(rate_s)
  integrated_rate <- .FindIntegral(
    initial_rate_s,
    initial_rate_h)

  calendar_ages <- stats::runif(
    200,
    min = min(initial_rate_s),
    max = max(initial_rate_s))

  for(i in 1:1000) {
    return_val <- .ChangePos(
      calendar_ages = calendar_ages,
      rate_s = rate_s,
      rate_h = rate_h,
      integrated_rate = integrated_rate)

    rate_s <- return_rate_s <- return_val$s
    integrated_rate <- return_integrated_rate <- return_val$integrated_rate
  }

  # Tests that changepoints are strictly increasing
  expect_true( all(diff(return_rate_s) > 0) )

  # Tests that number of changepoints hasn't varied
  expect_equal(
    length(return_rate_s),
    n_changepoints)

  # Test that first and last value of rate_s have remained same
  expect_equal(return_rate_s[1], initial_rate_s[1])
  expect_equal(
    return_rate_s[n_changepoints],
    initial_rate_s[n_changepoints])

  # Test that have updated integrated rate correctly
  expect_equal(
    return_integrated_rate,
    .FindIntegral(return_rate_s, rate_h)
  )

  # Test as to whether it has updated the positions
  expect_false(identical(return_rate_s, initial_rate_s))

})

