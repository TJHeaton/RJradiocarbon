#' Plots the posterior change points
#'
#' Add description
#'
#' @param output_data The return value from the updating functions
#' [RJradiocarbon::PPcalibrate]. Optionally, the output data can have an extra list item
#' named `label` which is used to set the label on the plot legend.
#' @param n_changes Which changes to plot for
#' @param n_burn The number of samples required for burn-in - any samples before this
#' are not used in the calculation. If not given, the first half of the
#' MCMC chain is discarded. Note that the maximum
#' value that can be chosen is `n_iter - 100 * n_thin` (where `n_iter` and `n_thin` are the
#' arguments given to [RJradiocarbon::PPcalibrate]).
#' @param n_end The iteration number of the last sample to use. Assumed to be the number of iterations
#' if not given.
#' @param kernel_bandwith The bandwidth used for the KDE of the density (optional). If not give 1/50th of the
#' calendar age range will be used.
#'
#'
#' @export
#'
#' @examples
#' # Plot results for a single calibration
#' # TODO
PlotPosteriorChangePoints <- function(
    output_data,
    n_changes = c(1, 2, 3),
    n_burn = NA,
    n_end = NA,
    kernel_bandwith = NA) {

  n_iter <- output_data$input_parameters$n_iter
  n_thin <- output_data$input_parameters$n_thin

  arg_check <- .InitializeErrorList()
  .CheckRJPPOutputData(arg_check, output_data)
  .CheckIntegerVector(arg_check, n_changes, lower = 1, upper = 4, max_length = 4)
  .CheckInteger(arg_check, n_posterior_samples, lower = 10)
  .CheckIntervalWidth(arg_check, interval_width, bespoke_probability)
  .CheckNBurnAndNEnd(arg_check, n_burn, n_end, n_iter, n_thin)
  .ReportErrors(arg_check)

  n_burn <- .SetNBurn(n_burn, n_iter, n_thin)
  n_end <- .SetNEnd(n_end, n_iter, n_thin)

  max_density <- 0
  all_densities <- list()

  posterior_n_internal_changes <- output_data$n_internal_changes[n_burn:n_end]
  posterior_rate_s <- output_data$rate_s[n_burn:n_end]

  cal_age_range <- sort(output_data$input_parameters$pp_cal_age_range)
  if (is.na(kernel_bandwith)) kernel_bandwith <- diff(cal_age_range) / 50

  colors <- c("blue", "darkgreen", "red", "purple")
  legend <- NULL

  for (n_change in n_changes) {
    legend <- c(legend, paste(n_change, "internal changes"))

    index <- which(posterior_n_internal_changes == n_change)
    if(length(index) == 0) next

    extracted_posteriors <- do.call(rbind, posterior_rate_s[index])
    for (j in 2:(n_change + 1)) {
      tryCatch({
        smoothed_density <- stats::density(
          extracted_posteriors[, j], bw = kernel_bandwith, from = cal_age_range[1], to = cal_age_range[2])
        if (max(smoothed_density$y) > max_density) max_density <- max(smoothed_density$y)
        this_line <- list(
          x = smoothed_density$x, y = smoothed_density$y, n_change = n_change, j = j
        )
        all_densities  <- append(all_densities, list(this_line))
      },
      error = function(cond) {
        message(paste("Could not calculate density for n_change =", n_change, ", j =",j, ": ", conditionMessage(cond)))
      })
    }
  }

  plot(
    x = NA,
    y = NA,
    xlim = rev(cal_age_range),
    ylim = c(0, max_density * 1.2),
    xlab = "Calendar Age BP",
    ylab = "Density",
    type = "n",
  )
  for (line in all_densities) {
    graphics::lines(line$x, line$y, lty = line$n_change, col = colors[line$n_change], lwd = 2)
  }
  legend("topright", legend = legend, lty = n_changes, col = colors)
  invisible(all_densities)
}