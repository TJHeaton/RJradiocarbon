test_that("PPcalibrate gives expected outcomes", {
  set.seed(13)
  n_observed <- 100
  n_iter <- 1000
  n_thin <- 10

  # Create artificial rc_determinations
  calendar_age_range <- c(0,10000)
  observed_age_range <- c(0,2000)
  true_theta <- runif(n_observed,
                      min = observed_age_range[1],
                      max = observed_age_range[2])

  rc_sigma <- rep(30, n_observed)
  rc_determinations <- rnorm(n = n_observed,
                             mean = approx(
                               x = intcal20$calendar_age_BP,
                               y = intcal20$c14_age,
                               xout = true_theta)$y,
                             sd = rc_sigmas
  )

  prior_n_internal_changepoints_lambda <- 5
  k_max_internal_changepoints <- 30
  rescale_factor_rev_jump <- 0.9
  default_prior_h_rate <- 0.1
  initial_n_internal_changepoints <- 10

  F14C_inputs <- FALSE
  use_F14C_space <- FALSE

  calibration_curve <- intcal20
  calendar_grid_resolution <- 10
  show_progress <- TRUE


  PP_fit_output <- PPcalibrate(
    rc_determinations = rc_determinations,
    rc_sigmas = rc_sigmas,
    calibration_curve = calibration_curve,
    F14C_inputs = F14C_inputs,
    n_iter = n_iter,
    n_thin = n_thin,
    use_F14C_space = use_F14C_space,
    show_progress = show_progress,
    calendar_grid_resolution = calendar_grid_resolution,
    prior_n_internal_changepoints_lambda = prior_n_internal_changepoints_lambda,
    k_max_internal_changepoints = k_max_internal_changepoints,
    rescale_factor_rev_jump = rescale_factor_rev_jump,
    default_prior_h_rate = default_prior_h_rate,
    initial_n_internal_changepoints = initial_n_internal_changepoints
  )

  post_n_internal_changes <- PP_fit_output$n_internal_changes
  post_rate_h <-

  expect_identical(n_changes - 1L, n_heights)



})
