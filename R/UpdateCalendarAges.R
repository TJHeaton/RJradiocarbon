#' Title
#'
#' @param likelihood_from_calibration_curve
#' @param calendar_age_grid
#' @param rate_s
#' @param rate_h
#'
#' @return
#' @export
#'
#' @examples
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





# Find likelihood (given calibration curve) of:
# vector of calendar ages theta for a single 14C observation
.CalendarAgeLikelihoodGivenCurve <- function(
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

  cal_curve_mu <- approx(x = calibration_curve$calendar_age,
                         y = calcurve_rc_ages,
                         xout = theta)$y
  cal_curve_sigma <- approx(x = calibration_curve$calendar_age,
                            y = calcurve_rc_sigs,
                            xout = theta)$y

  dnorm(rc_determination, cal_curve_mu, sqrt(cal_curve_sigma^2 + rc_sigma^2))
}




# Find (un-normalised) prior on theta (on a specific calendar grid) for a given Poisson Process
.FindCalendarAgePriorGivenPoissonProcess <- function(
    rate_s,
    rate_h,
    theta)
{
  # Prior is proportional to rate at given calendar age
  prior_theta <- approx(
    x = rate_s,
    y = c(rate_h, 0), # approx() interpolates from left
    xout = theta,
    method = "constant")$y

  return(prior_theta)
}

