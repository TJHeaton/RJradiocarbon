% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PPcalibrate.R
\name{PPcalibrate}
\alias{PPcalibrate}
\title{Title}
\usage{
PPcalibrate(
  rc_determinations,
  rc_sigmas,
  calibration_curve,
  F14C_inputs = FALSE,
  n_iter = 1e+05,
  n_thin = 10,
  use_F14C_space = TRUE,
  show_progress = TRUE,
  calendar_age_range = NA,
  calendar_grid_resolution = 1,
  prior_h_shape = NA,
  prior_h_rate = NA,
  prior_n_internal_changepoints_lambda = 10,
  k_max_internal_changepoints = 30,
  rescale_factor_rev_jump = 0.9,
  bounding_range_prob_cutoff = 0.005,
  default_prior_h_rate = 0.1,
  initial_n_internal_changepoints = 10,
  grid_extension_factor = 0.1,
  use_fast = TRUE
)
}
\arguments{
\item{rc_determinations}{A vector of observed radiocarbon determinations}

\item{rc_sigmas}{A vector of the radiocarbon determinations
uncertainties (1-sigma). Must be the same length as \code{rc_determinations}.}

\item{calibration_curve}{A dataframe which must contain one column \code{calendar_age_BP}, and also
columns \code{c14_age} and \code{c14_sig} or \code{f14c} and \code{f14c_sig} (or both sets).
This format matches the curves supplied with this package}

\item{F14C_inputs}{\code{TRUE} if the provided rc_determinations are F14C concentrations and \code{FALSE}
if they are radiocarbon age BP. Defaults to \code{FALSE}.}

\item{n_iter}{The number of MCMC iterations (optional). Default is 100,000.}

\item{n_thin}{How much to thin the output (optional). 1 is no thinning,
a larger number is more thinning. Default is 10. Must choose an integer more
than 1 and not too close to \code{n_iter}, to ensure there are enought samples from
posterior to potentially use.}

\item{use_F14C_space}{Whether the calculations are carried out in F14C space (default is TRUE).
If FALSE, calculations are carried out in 14C yr BP space.}

\item{show_progress}{Whether to show a progress bar in the console during
execution. Default is \code{TRUE}.}

\item{calendar_age_range}{Minimum and maximum calendar ages permitted
for the calendar ages of the samples, i.e. range_1 < theta < range_2.}

\item{calendar_grid_resolution}{The spacing of the calendar age grid on which to consider
the ages of the samples, e.g. t, t + resolution, t + 2 * resolution, ..}

\item{prior_h_shape, prior_h_rate}{Prior for Poisson Process rate height in any interval
rate_h ~ Gamma(shape = prior_h_shape, rate = prior_h_rate)
If choose sensible_initialisation then prior_h_shape is selected adaptively (internally)
to match n_observations (and selecting prior_h_rate = 0.1 to provide diffuse prior)}

\item{prior_n_internal_changepoints_lambda}{Prior on Poisson parameter specifying
n_internal_changepoints ~ Po(prior_n_internal_changepoints_lambda)}

\item{k_max_internal_changepoints}{Maximum permitted number of internal changepoints}

\item{rescale_factor_rev_jump}{Factor weighting probability of dimension change
in the reversible jump update step for poisson process rate_h and rate_s}

\item{bounding_range_prob_cutoff}{Probability cut-off for choosing the bounds for the
potential calendar ages for the observations}

\item{default_prior_h_rate}{Default value for rate parameter in Gamma prior for rate
if not specifically passed by user alongside prior_h_shape.
Important to be able to set independently as determines the diffusivity of the prior
Var(rate_h) = (1/default_prior_h_rate) * (n_determinations / calendar_age_interval_length)
Smaller values of default_prior_h_rate will mean more diverse prior}

\item{initial_n_internal_changepoints}{Number of internal changepoints to initialise
the sampler with. The default is 10 (so diffuse).
Will place them randomly within calendar interval}

\item{grid_extension_factor}{If you adaptively select the calendar_age_range from
rc_determinations, how far you wish to extend the grid beyond this adaptive minimum and maximum
The final range will be extended (eqaully on both sides) to cover (1 + grid_extension_factor) * (calendar_age_range)}

\item{use_fast}{Adding a flag to allow trimming the likelihoods based on prob cutoff}
}
\value{
TODO
}
\description{
Title
}
\examples{
# TODO
}
