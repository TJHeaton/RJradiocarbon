.ProbabilitiesForSingleDetermination <- function(
    rc_determination, rc_sigma, F14C_inputs, calibration_curve) {

  if (F14C_inputs) {
    calibration_curve <- .AddF14cColumns(calibration_curve)
    calcurve_rc_ages <- calibration_curve$f14c
    calcurve_rc_sigs <- calibration_curve$f14c_sig
  } else {
    calibration_curve <- .AddC14ageColumns(calibration_curve)
    calcurve_rc_ages <- calibration_curve$c14_age
    calcurve_rc_sigs <- calibration_curve$c14_sig
  }

  probabilities <- stats::dnorm(
    rc_determination, mean=calcurve_rc_ages, sd=sqrt(calcurve_rc_sigs^2 + rc_sigma^2))
  probabilities <- probabilities / sum(probabilities)
  return(probabilities)
}

.AddF14cColumns <- function(data) {
  if (any(names(data) == "f14c") && any(names(data) == "f14c_sig")) {
    return(data)
  }
  data$f14c <- exp(-data$c14_age / 8033)
  data$f14c_sig <- data$f14c * data$c14_sig / 8033
  return(data)
}


.AddC14ageColumns <- function(data) {
  if (any(names(data) == "c14_age") && any(names(data) == "c14_sig")) {
    return(data)
  }
  data$c14_age <- -8033 * log(data$f14c)
  data$c14_sig <- 8033 * data$f14c_sig / data$f14c
  return(data)
}
