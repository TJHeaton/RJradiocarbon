UpdateCalendarAges <- function(
    likelihood_from_calibration_curve,
    calendar_age_grid,
    rate_s,
    rate_h)
{
  priort <- findcalprior(s, h, t)
  # Multiply each column of likelihood by prior
  postt <- sweep(tprob, MARGIN=1, priort, `*`)


  # TO DO

}



# Find likelihood of vector of calendar ages theta for a single 14C observation
.CalendarLikelihoodGivenCurve <- function(
    rc_determination,
    rc_sigma,
    theta,
    F14C_inputs,
    calibration_curve)
{
  if (F14C_inputs) {
    # calibration_curve <- .AddF14cColumns(calibration_curve) # TODO
    calcurve_rc_ages <- calibration_curve$f14c
    calcurve_rc_sigs <- calibration_curve$f14c_sig
  } else {
    # calibration_curve <- .AddC14ageColumns(calibration_curve) # TODO
    calcurve_rc_ages <- calibration_curve$c14_age
    calcurve_rc_sigs <- calibration_curve$c14_sig
  }

  cal_mu <- approx(calibration_curve$calendar_age, calcurve_rc_ages, theta)$y
  cal_sigma <- approx(calibration_curve$calendar_age, calcurve_rc_sigs, theta)$y

  lik <- dnorm(rc_determination, cal_mu, sqrt(cal_sigma^2 + rc_sigma^2))
  return(lik)
}

# Find prior on theta (on a specific calendar grid) for a given Poisson Process
.FindCalPrior <- function(
    rate_s,
    rate_h,
    theta)
  {
  # Prior is proportional to rate at given calendar age
  prior_calendar_ages <- approx(
    x = rate_s,
    y = c(rate_h, 0), # approx() interpolates from left
    xout = theta,
    method = "constant")$y

  return(prior_theta)
}
