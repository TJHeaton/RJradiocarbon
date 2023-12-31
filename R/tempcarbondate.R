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

.AddF14cColumns <- function(data) {
  if (any(names(data) == "f14c") && any(names(data) == "f14c_sig")) {
    return(data)
  }
  data$f14c <- exp(-data$c14_age / 8033)
  data$f14c_sig <- data$f14c * data$c14_sig / 8033
  return(data)
}


.Convert14CageToF14c <- function(c14_determinations, c14_sigmas) {

  f14c <- exp(-c14_determinations / 8033)
  f14c_sig <- f14c * c14_sigmas / 8033

  return(data.frame(f14c = f14c, f14c_sig = f14c_sig))
}


.ConvertF14cTo14Cage <- function(f14c_determinations, f14c_sigmas) {

  c14_age <- -8033 * log(f14c_determinations)
  c14_sig <- 8033 * f14c_sigmas / f14c_determinations

  return(data.frame(c14_age = c14_age, c14_sig = c14_sig))
}


.SetNOut <- function (n_iter, n_thin) {
  n_out <- floor(n_iter / n_thin) + 1
}


.SetNBurn <- function(n_burn, n_iter, n_thin) {
  if (is.na(n_burn)) {
    n_burn <- floor(n_iter / (2 * n_thin))
  } else {
    n_burn <- floor(n_burn / n_thin)
  }
  return(n_burn)
}


.SetNEnd <- function(n_end, n_iter, n_thin) {
  if (is.na(n_end)) {
    n_end <- .SetNOut(n_iter, n_thin)
  } else {
    n_end <- floor(n_end / n_thin)
  }
  return(n_end)
}