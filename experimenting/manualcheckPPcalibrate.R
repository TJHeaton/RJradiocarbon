library(boot)
set.seed(13)


true_theta <- as.vector(coal[,1])
true_theta <- 365 * (true_theta - min(true_theta))
n_observed <- length(true_theta)

rc_sigmas <- rep(30, n_observed)
rc_determinations <- rnorm(
  n = n_observed,
  mean = approx(
    x = intcal20$calendar_age_BP,
    y = intcal20$c14_age,
    xout = true_theta)$y,
  sd = rc_sigmas
)

calendar_grid_resolution <- 100
calendar_age_range <- c(0,50000)
rate_s <- NA
rate_h <- NA
prior_n_change_lambda <- 5
k_max_changes <- 30
rescale_factor_rev_jump <- 0.9
n_iter <- 10000
F14C_inputs <- FALSE
calibration_curve <- intcal20

rm(true_theta, n_observed)
