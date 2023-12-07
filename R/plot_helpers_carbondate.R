
.SetUpDensityPlot <- function(plot_AD, xlim, ylim) {
  graphics::par(new = TRUE)

  if (plot_AD) xlim <- 1950 - xlim
  graphics::plot.default(
    NULL,
    NULL,
    type = "n",
    ylim = ylim,
    xlim = xlim,
    axes = FALSE,
    xlab = NA,
    ylab = NA)
}


.PlotSPDEstimateOnCurrentPlot <- function(plot_AD, SPD, SPD_colour) {
  if (plot_AD) {
    cal_age <- 1950 - SPD$calendar_age
  } else {
    cal_age <- SPD$calendar_age
  }

  graphics::polygon(
    c(cal_age, rev(cal_age)),
    c(SPD$probability, rep(0, length(SPD$probability))),
    border = NA,
    col = SPD_colour)
}


.PlotDensityEstimateOnCurrentPlot <- function(
    plot_AD, predictive_density, output_colour, show_confidence_intervals) {

  if (plot_AD) {
    cal_age <- 1950 - predictive_density$calendar_age
  } else {
    cal_age <- predictive_density$calendar_age
  }

  graphics::lines(cal_age, predictive_density$density_mean, col = output_colour)
  if (show_confidence_intervals) {
    graphics::lines(cal_age, predictive_density$density_ci_lower, col = output_colour, lty = 2)
    graphics::lines(cal_age, predictive_density$density_ci_upper, col = output_colour, lty = 2)
  }
}


.AddLegendToDensityPlot <- function(
    output_data,
    show_SPD,
    show_confidence_intervals,
    interval_width,
    bespoke_probability,
    calibration_curve_colour,
    output_colours,
    SPD_colour) {

  ci_label <- switch(
    interval_width,
    "1sigma" = expression(paste(sigma, " interval")),
    "2sigma"  = expression(paste("2", sigma, " interval")),
    "bespoke" = paste0(round(100 * bespoke_probability), "% interval"))

  legend_labels <- c(
    gsub("intcal", "IntCal", output_data[[1]]$input_data$calibration_curve_name),
    ci_label)
  lty <- c(1, 2)
  pch <- c(NA, NA)
  col <- c(calibration_curve_colour, calibration_curve_colour)

  for (i in seq_along(output_data)) {
    legend_labels <- c(legend_labels, output_data[[i]]$label)
    lty <- c(lty, 1)
    pch <- c(pch, NA)
    col <- c(col, output_colours[[i]])

    if (show_confidence_intervals) {
      legend_labels <- c(legend_labels, ci_label)
      lty <- c(lty, 2)
      pch <- c(pch, NA)
      col <- c(col, output_colours[[i]])
    }
  }

  if (show_SPD) {
    legend_labels <- c(legend_labels, "SPD Estimate")
    lty <- c(lty, -1)
    pch <- c(pch, 15)
    col <- c(col, SPD_colour)
  }

  graphics::legend(
    "topright", legend = legend_labels, lty = lty, pch = pch, col = col)
}
