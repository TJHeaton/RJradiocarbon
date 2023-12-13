#' Plots the posterior mean rate from the output data
#'
#' Plots the input radiocarbon determinations and calibration curve, with the
#' output posterior mean rate on the same plot. Can also optionally show the
#' individual mean calendar ages.
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
PlotPosteriorChangePoints <- function(
    output_data,
    n_changes = c(1, 2, 3),
    n_burn = NA,
    n_end = NA) {

  n_thin <- output_data$input_parameters$n_thin
  n_out <- length(output_data$n_internal_changes)

  if (is.na(n_burn)) { n_burn <- floor(n_out / 2) } else { n_burn <- floor(n_burn / n_thin) }
  if (is.na(n_end)) { n_end <- n_out } else { n_end <- floor(n_end / n_thin) }
  # TODO Should we be using n_burn and n_end?

  max_density <- 0
  min_x <- NA
  max_x <- NA
  all_densities <- list()

  posterior_n_internal_changes <- output_data$n_internal_changes[n_burn:n_end]
  posterior_rate_s <- output_data$rate_s[n_burn:n_end]

  bandwidth  <- diff(output_data$input_parameters$pp_cal_age_range) / 20

  colors <- c("blue", "purple", "red")

  for (n_change in n_changes) {

    index <- which(posterior_n_internal_changes == n_change)

    if(length(index) == 0) next

    extracted_posteriors <- do.call(rbind, posterior_rate_s[index])

    for (j in 2:(n_change + 1)) {
      tryCatch({
        smoothed_density <- stats::density(extracted_posteriors[, j], bw = bandwidth)
        if (max(smoothed_density$y) > max_density) max_density <- max(smoothed_density$y)
        graphics::lines(smoothed_density, lwd = n_change, col = colors[n_change])
        this_line <- list(
          x = smoothed_density$x, y = smoothed_density$y, n_change = n_change, j = j
        )
        all_densities  <- append(all_densities, this_line)
      },
      error = function(cond) {
        message(paste("Could not calculate density for:", n_change, j, " - ", conditionMessage(cond)))
      })

    }
  }
}