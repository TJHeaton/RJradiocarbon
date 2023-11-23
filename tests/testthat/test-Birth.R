test_that("Birth gives expected outcomes", {

  set.seed(14)
  rate_s <- initial_rate_s <- c(0, 5, 10)
  rate_h <- initial_rate_h <- c(5, 2)
  n_heights <- length(rate_h)
  integrated_rate <- .FindIntegral(
    initial_rate_s,
    initial_rate_h)
  prior_h_alpha <- 4
  prior_h_beta <- 1
  prior_n_change_lambda <- 1000
  prop_birth_ratio <- 0.1

  calendar_ages <- stats::runif(
    200,
    min = min(initial_rate_s),
    max = max(initial_rate_s))

  n_iters <- 10000

  n_changes <- rep(NA, n_iters)
  n_heights <- rep(NA, n_iters)
  are_heights_positive <- rep(NA, n_iters)
  are_changepoints_increasing <- rep(NA, n_iters)
  are_changepoints_bounds_correct <- rep(NA, n_iters)
  are_rate_lengths_compatible <- rep(NA, n_iters)

  set.seed(11)
  for(i in 1:n_iters) {
    return_val <- .Birth(
      theta = calendar_ages,
      rate_s = rate_s,
      rate_h = rate_h,
      integrated_rate = integrated_rate,
      prior_h_alpha = prior_h_alpha,
      prior_h_beta = prior_h_beta,
      prior_n_change_lambda = prior_n_change_lambda,
      proposal_ratio = prop_birth_ratio)

    rate_h <- return_rate_h <- return_val$rate_h
    rate_s <- return_rate_s <- return_val$rate_s
    integrated_rate <- return_integrated_rate <- return_val$integrated_rate

    n_changes[i] <- length(rate_s)
    n_heights[i] <- length(rate_h)

    are_heights_positive[i] <- all(return_rate_h >= 0)
    are_changepoints_increasing[i] <- all(diff(return_rate_s) > 0)
    are_changepoints_bounds_correct[i] <- (
      (min(return_rate_s) == min(initial_rate_s)) &&
      (max(return_rate_s) == max(initial_rate_s))
    )
  }

  # Tests that heights are all strictly positive
  expect_true( all(are_heights_positive) )

  # Tests that changepoint locations are strictly increasing
  expect_true( all(are_changepoints_increasing) )

  # Tests that keeps same initial and final changepoints
  expect_true( all(are_changepoints_bounds_correct) )

  # Tests that number of height is always one less than number of changepoints
  expect_identical(n_changes - 1L, n_heights)

  # Test that have updated integrated rate correctly
  expect_equal(
    return_integrated_rate,
    .FindIntegral(return_rate_s, return_rate_h)
  )

  # Test as to whether it has updated the heights and changepoints
  # Whether is passes or fails will depend upon seed and initialisation point
  # This version should pass (as accepts some changes)
  expect_false(identical(return_rate_h, initial_rate_h))
  expect_false(identical(return_rate_s, initial_rate_s))

  # That each running either increases or keeps number changepoints the same
  expect_true(all(diff(n_changes) == 0 | diff(n_changes) == 1))

})


### Second test
test_that("Birth gives same as legacy code", {

  set.seed(14)
  rate_s <- initial_rate_s <- c(0, 5, 10)
  rate_h <- initial_rate_h <- c(5, 2)
  n_heights <- length(rate_h)
  integrated_rate <- initial_integrated_rate <- .FindIntegral(
    initial_rate_s,
    initial_rate_h)
  prior_h_alpha <- 1
  prior_h_beta <- 100
  prior_n_change_lambda <- 1000
  prop_birth_ratio <- 0.1

  calendar_ages <- stats::runif(
    200,
    min = min(initial_rate_s),
    max = max(initial_rate_s))

  n_iters <- 10000
  hastings_ratio_new <- rep(NA, n_iters)
  hastings_ratio_legacy <- rep(NA, n_iters)


  # New code
  set.seed(11)
  for(i in 1:n_iters) {
    return_val <- .Birth(
      theta = calendar_ages,
      rate_s = rate_s,
      rate_h = rate_h,
      integrated_rate = integrated_rate,
      prior_h_alpha = prior_h_alpha,
      prior_h_beta = prior_h_beta,
      prior_n_change_lambda = prior_n_change_lambda,
      proposal_ratio = prop_birth_ratio)

    rate_h <- return_rate_h <- return_val$rate_h
    rate_s <- return_rate_s <- return_val$rate_s
    integrated_rate <- return_integrated_rate <- return_val$integrated_rate

    hastings_ratio_new[i] <- return_val$hastings_ratio
  }

  # Store final output of new code
  final_integrated_rate_new <- integrated_rate
  final_rate_h_new <- rate_h
  final_rate_s_new <- rate_s

  # Legacy
  source(test_path("fixtures", "LegacyBirth.R"))
  set.seed(11)

  rate_s <- initial_rate_s
  rate_h <- initial_rate_h
  integrated_rate <- initial_integrated_rate

  for(i in 1:n_iters) {
    return_val <- LegacyBirth(
      th = calendar_ages,
      s = rate_s,
      h = rate_h,
      intrate = integrated_rate,
      alpha = prior_h_alpha,
      beta = prior_h_beta,
      lambda = prior_n_change_lambda,
      propratio = prop_birth_ratio)

    previous_n_change <- length(rate_h)
    rate_h <- return_rate_h <- return_val$h
    rate_s <- return_rate_s <- return_val$s
    integrated_rate <- return_integrated_rate <- return_val$intrate

    hastings_ratio_legacy[i] <- return_val$hastings_ratio
  }

  # Test that hastings ratios are identical
  expect_equal(hastings_ratio_new,
               hastings_ratio_legacy)

  # Test final rates and integral are identical
  expect_identical(return_integrated_rate, final_integrated_rate_new)
  expect_identical(return_rate_h, final_rate_h_new)
  expect_identical(return_rate_s, final_rate_s_new)

})
