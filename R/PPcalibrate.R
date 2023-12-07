#' Title
#'
#' @param rc_determinations A vector of observed radiocarbon determinations
#'
#' @param rc_sigmas A vector of the radiocarbon determinations
#' uncertainties (1-sigma). Must be the same length as `rc_determinations`.
#'
#' @param calibration_curve A dataframe which must contain one column `calendar_age_BP`, and also
#' columns `c14_age` and `c14_sig` or `f14c` and `f14c_sig` (or both sets).
#' This format matches the curves supplied with this package
#'
#' @param F14C_inputs `TRUE` if the provided rc_determinations are F14C concentrations and `FALSE`
#' if they are radiocarbon age BP. Defaults to `FALSE`.
#'
#' @param n_iter  The number of MCMC iterations (optional). Default is 100,000.
#'
#' @param n_thin  How much to thin the output (optional). 1 is no thinning,
#' a larger number is more thinning. Default is 10. Must choose an integer more
#' than 1 and not too close to `n_iter`, to ensure there are enought samples from
#' posterior to potentially use.
#'
#' @param use_F14C_space Whether the calculations are carried out in F14C space (default is TRUE).
#' If FALSE, calculations are carried out in 14C yr BP space.
#'
#' @param show_progress Whether to show a progress bar in the console during
#' execution. Default is `TRUE`.
#'
#' @param sensible_initialisation Whether to use sensible start values and
#' adaptive prior on \eqn{\mu_{\phi}} and  (A, B).
#' If this is `TRUE` (the default), then all the remaining arguments below are
#' ignored.
#'
#' @param calendar_grid_resolution The spacing of the calendar age grid on which to consider
#' the ages of the samples, e.g. t, t + resolution, t + 2 * resolution, ..
#'
#' @param calendar_age_range Minimum and maximum calendar ages permitted
#' for the calendar ages of the samples, i.e. range_1 < theta < range_2.
#' Required if `sensible_initialisation` is `FALSE`.
#'
#' @param rate_s,rate_h Initial parameters to define/specify the poisson process rate
#' (jumps and heights).
#'
#' @param prior_n_internal_changes_lambda Prior on Poisson parameter specifying
#' n_internal_changepoints ~ Po(prior_n_internal_changes_lambda)
#'
#' @param prior_h_rate Prior for Poisson Process rate height in any interval
#' rate_h ~ Gamma(shape = prior_h_shape, rate = prior_h_rate)
#' prior_h_shape is chosen adaptively/internally to match n_observations
#'
#' @param k_max_internal_changepoints Maximum permitted number of internal changepoints
#'
#' @param rescale_factor_rev_jump Factor weighting probability of dimension change
#' in the reversible jump update step for poisson process rate_h and rate_s
#'
#' @param calendar_ages  The initial estimate for the underlying calendar ages
#' (optional). If supplied it must be a vector with the same length as
#' `rc_determinations`.  Required if `sensible_initialisation` is `FALSE`.
#'
#'
#' @return TODO
#' @export
#'
#' @examples # TODO
PPcalibrate <- function(
    rc_determinations,
    rc_sigmas,
    calibration_curve,
    F14C_inputs = FALSE,
    n_iter = 1e5,
    n_thin = 10,
    use_F14C_space = TRUE,
    show_progress = TRUE,
    sensible_initialisation = TRUE,
    calendar_grid_resolution = 1,
    calendar_age_range = NA,
    rate_s = NA, rate_h = NA,
    prior_n_internal_changes_lambda = NA,
    prior_h_rate = 0.1,
    k_max_internal_changepoints = NA,
    rescale_factor_rev_jump = 0.9,
    calendar_ages = NA) {

  ##############################################################################
  # Save input data
  input_data <- list(
    rc_determinations = rc_determinations,
    rc_sigmas = rc_sigmas,
    F14C_inputs = F14C_inputs,
    calibration_curve_name = deparse(substitute(calibration_curve)))




  # Find initial calendar_age_range
  if(sensible_initialisation) {
    ##############################################################################
    ## Interpolate cal curve onto single year (regular) grid
    ## Must be regular calendar grid for individual_possible_calendar_ranges
    ## which works with normalised vector of probabilities
    integer_cal_year_curve <- carbondate::InterpolateCalibrationCurve(NA, calibration_curve, use_F14C_space)
    interpolated_calendar_age_start <- integer_cal_year_curve$calendar_age_BP[1]
    if (use_F14C_space) {
      interpolated_rc_age <- integer_cal_year_curve$f14c
      interpolated_rc_sig <- integer_cal_year_curve$f14c_sig
    } else {
      interpolated_rc_age <- integer_cal_year_curve$c14_age
      interpolated_rc_sig <- integer_cal_year_curve$c14_sig
    }

    individual_possible_calendar_ranges <- mapply(
      .FindCalendarRangeForSingleDetermination,
      rc_determinations,
      rc_sigmas,
      MoreArgs = list(
        F14C_inputs=use_F14C_space,
        calibration_curve=integer_cal_year_curve,
        prob_cutoff = 0.005))

    min_potential_calendar_age <- min(
      individual_possible_calendar_ranges[1,])
    max_potential_calendar_age <- max(
      individual_possible_calendar_ranges[2,])
  } else {
    ####################################
    ## Create calendar_age_grid covering potential calendar ages
    min_potential_calendar_age <- min(calendar_age_range)
    max_potential_calendar_age <- max(calendar_age_range)
  }

  calendar_age_grid <- seq(
    min_potential_calendar_age,
    max_potential_calendar_age,
    by = calendar_grid_resolution) # May stop before max_potential_calendar_age (so check needed)

  # Ensure end of calendar_age_grid extends at least to max_potential_calendar_age
  # If not extend calendar_age_grid and adjust max_potential_calendar_age so values match
  if(max(calendar_age_grid) != max_potential_calendar_age) {
    max_potential_calendar_age <- (
      calendar_age_grid[length(calendar_age_grid)] + calendar_grid_resolution
    )
    calendar_age_grid <- c(calendar_age_grid,
                           max_potential_calendar_age)
  }

  calendar_age_interval_length <- max_potential_calendar_age - min_potential_calendar_age

  if(sensible_initialisation) {
    ####################################
    ## Create initial values for hyperparameters on Poisson process rate
    n_determinations <- length(rc_determinations)
    initial_estimate_mean_rate <- n_determinations / calendar_age_interval_length

    prior_h_shape <- initial_estimate_mean_rate / prior_h_rate

    ## Create initial change points and heights for Poisson process rTE
    initial_n_internal_change <- 10
    initial_rate_s <- sort(
      c(
        min_potential_calendar_age,
        stats::runif(initial_n_internal_change,
              min = min_potential_calendar_age,
              max = max_potential_calendar_age),
        max_potential_calendar_age
      )
    )
    initial_rate_h <- stats::rgamma(
      n = initial_n_internal_change + 1,
      shape =  prior_h_shape,
      rate = prior_h_rate
    )

    initial_integrated_rate <- .FindIntegral(
      rate_s = initial_rate_s,
      rate_h = initial_rate_h
    )
  }


  ##############################################################################
  # Save input parameters
  input_parameters <- list(
    pp_cal_age_range = c(min_potential_calendar_age,
                         max_potential_calendar_age),
    prior_n_internal_changes_lambda = prior_n_internal_changes_lambda,
    k_max_internal_changepoints = k_max_internal_changepoints,
    prior_h_shape = prior_h_shape,
    prior_h_rate = prior_h_rate,
    rescale_factor_rev_jump = rescale_factor_rev_jump,
    n_iter = n_iter,
    n_thin = n_thin)

  ####################################
  ## Create matrix of calendar_likelihoods (stored in main as not updated throughout samples)
  ## Different from .ProbabilitiesForSingleDetermination as not normalised
  ## and can pass theta on different grid to calibration_curve
  likelihood_calendar_ages_from_calibration_curve <- mapply(
    .CalendarAgeLikelihoodGivenCurve,
    rc_determinations,
    rc_sigmas,
    MoreArgs = list(
      theta = calendar_age_grid,
      F14C_inputs = F14C_inputs,
      calibration_curve = calibration_curve)
  )

  num_observations <- length(rc_determinations)

  # Set starting values to be initialised ones
  rate_s <- initial_rate_s
  rate_h <- initial_rate_h
  integrated_rate <- initial_integrated_rate

  prob_move <- .FindMoveProbability(
    prior_n_internal_changes_lambda = prior_n_internal_changes_lambda,
    k_max_internal_changepoints = k_max_internal_changepoints,
    rescale_factor = rescale_factor_rev_jump)

  ####################################
  # Create storage for output
  n_out <- floor(n_iter / n_thin)

  rate_s_out <- list(rate_s)
  rate_h_out <- list(rate_h)
  n_internal_changes <- rep(NA, length = n_out)
  theta_out <- matrix(NA, nrow = n_out, ncol = num_observations)

  output_index <- 0

  #####################################
  # Perform MCMC - RJMCMC within Gibbs
  # Consist of iterating between:
  #    i) Updating calendar_ages given lambda (rate_s, rate_h) and rc_determinations
  #    ii) Updating lambda (rate_s, rate_h) given calendar_ages using RJ MCMC

  if (show_progress) {
    progress_bar <- utils::txtProgressBar(min = 0, max = n_iter, style = 3)
  }


  for(iter in 1:n_iter) {

    ## Step 1: Update calendar_ages given rate_s and rate_h (sample from exactly using Gibbs)
    calendar_ages <- UpdateCalendarAgesGibbs(
      likelihood_calendar_ages_from_calibration_curve = likelihood_calendar_ages_from_calibration_curve,
      calendar_age_grid = calendar_age_grid,
      rate_s = rate_s,
      rate_h = rate_h
    )

    ## Step 2: Update rate_s and rate_h given current calendar_ages (using RJMCMC)
    updated_poisson_process <- UpdatePoissonProcessRateRevJump(
      theta = calendar_ages,
      rate_s = rate_s,
      rate_h = rate_h,
      integrated_rate = integrated_rate,
      prior_h_shape = prior_h_shape,
      prior_h_rate = prior_h_rate,
      prior_n_internal_changes_lambda = prior_n_internal_changes_lambda,
      prob_move = prob_move
    )

    rate_s <- updated_poisson_process$rate_s
    rate_h <- updated_poisson_process$rate_h
    integrated_rate <- updated_poisson_process$integrated_rate

    # Store output
    if (iter %% n_thin == 0) {
      output_index <- output_index + 1
      n_internal_changes[output_index] <- length(rate_h) - 1
      rate_h_out[[output_index]] <- rate_h
      rate_s_out[[output_index]] <- rate_s
      theta_out[output_index, ] <- calendar_ages
    }

    if (show_progress) {
      if (iter %% 100 == 0) {
        utils::setTxtProgressBar(progress_bar, iter)
      }
    }
  }

  return_list <- list(
    rate_s = rate_s_out,
    rate_h = rate_h_out,
    calendar_ages = theta_out,
    n_internal_changes = n_internal_changes,
    input_data = input_data,
    input_parameters = input_parameters)

  if (show_progress) close(progress_bar)
  return(return_list)
}

## TODO - Think about how to choose min and max cut-off currently ends where there is an observation so will have higher rate
