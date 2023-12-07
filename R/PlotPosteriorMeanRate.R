#' Plots the posterior mean rate from the output data
#'
#' Plots the input radiocarbon determinations and calibration curve, with the
#' output posterior mean rate on the same plot. Can also optionally show the
#' individual mean calendar ages.
#'
#' @param output_data The return value from the updating functions
#' [RJradiocarbon::PPcalibrate]. Optionally, the output data can have an extra list item
#' named `label` which is used to set the label on the plot legend.
#' @param n_posterior_samples Current number of samples it will draw from this
#' posterior to estimate the calendar age density (possibly repeats). If not
#' given 5000 is used.
#' @param calibration_curve This is usually not required since the name of the
#' calibration curve variable is saved in the output data. However if the
#' variable with this name is no longer in your environment then you should pass
#' the calibration curve here. If provided this should be a dataframe which
#' should contain at least 3 columns entitled calendar_age, c14_age and c14_sig.
#' This format matches [carbondate::intcal20].
#' @param plot_14C_age Whether to use the 14C yr BP as the units of the y-axis.
#' Defaults to TRUE. If FALSE uses F14C concentration instead.
#' @param show_individual_means Whether to calculate and show the individual mean
#' calendar ages on the plot (optional). Default is `FALSE`.
#' @param show_confidence_intervals Whether to show the confidence intervals
#' for the chosen probability on the plot. Default is `TRUE`.
#' @param interval_width The confidence intervals to show for both the
#' calibration curve and the predictive density. Choose from one of `"1sigma"` (68.3%),
#' `"2sigma"` (95.4%) and `"bespoke"`. Default is `"2sigma"`.
#' @param bespoke_probability The probability to use for the confidence interval
#' if `"bespoke"` is chosen above. E.g. if 0.95 is chosen, then the 95% confidence
#' interval is calculated. Ignored if `"bespoke"` is not chosen.
#' @param denscale Whether to scale the vertical range of the density plot
#' relative to the calibration curve plot (optional). Default is 3 which means
#' that the maximum SPD density will be at 1/3 of the height of the plot.
#' @param n_calc Number of points to use when calculating the predictive
#' density. Default is 1001.
#' @param n_burn The number of samples required for burn-in - any samples before this
#' are not used in the calculation. If not given, the first half of the
#' MCMC chain is discarded. Note that the maximum
#' value that can be chosen is `n_iter - 100 * n_thin` (where `n_iter` and `n_thin` are the
#' arguments given to [RJradiocarbon::PPcalibrate]).
#' @param n_end The iteration number of the last sample to use. Assumed to be the number of iterations
#' if not given.
#'
#'
#' @return A list, each item containing a data frame of the `calendar_age`, the `rate_mean`
#' and the confidence intervals for the rate - `rate_ci_lower` and `rate_ci_upper`.
#'
#' @export
#'
#' @examples
#' # Plot results for a single calibration
#' # TODO
PlotPosteriorMeanRate <- function(
    output_data,
    n_posterior_samples = 5000,
    calibration_curve = NULL,
    plot_14C_age = TRUE,
    show_individual_means = FALSE,
    show_confidence_intervals = TRUE,
    interval_width = "2sigma",
    bespoke_probability = NA,
    denscale = 3,
    n_calc = 1001,
    n_burn = NA,
    n_end = NA) {

}