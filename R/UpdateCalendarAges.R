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

