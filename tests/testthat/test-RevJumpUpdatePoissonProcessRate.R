test_that("RevJumpUpdatePoissonProcessRate gives expected outcomes", {
  set.seed(14)
  rate_s <- initial_rate_s <- c(0, 2, 5, 7, 10)
  rate_h <- initial_rate_h <- c(5, 2, 4, 6)

  # Choose sensible prior on h
  # Prior mean matches mean of rate_h
  prior_h_shape <- 0.1 * mean(rate_h)
  prior_h_rate <- 0.1 # Bit disperse

  n_sections <- length(rate_h)
  n_obs_per_section <- rpois(
    n = n_sections,
    lambda = rate_h * diff(rate_s))

  calendar_ages <- c()
  for(i in 1:n_sections) {
    calendar_ages <- c(
      calendar_ages,
      runif(n_obs_per_section,
            min = rate_s[i],
            max =rate_s[i+1])
    )
  }

  prior_n_change_lambda <- 6
  prob_move <-


  n_heights <- length(rate_h)
  integrated_rate <- .FindIntegral(
    initial_rate_s,
    initial_rate_h)

  n_theta <- 2 * integrated_rate # Double expected number

  calendar_ages <- stats::runif(
    n_theta,
    min = min(initial_rate_s),
    max = max(initial_rate_s))



})
