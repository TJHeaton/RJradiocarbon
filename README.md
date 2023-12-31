
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RJradiocarbon

<!-- badges: start -->
<!-- badges: end -->

Radiocarbon Calibration Of Multiple $^{14}$C Samples Using Reversible
Jump MCMC and a Poisson process

The goal of RJradiocarbon is to model the occurrence of radiocarbon
samples as a (variable rate) Poisson process. Given a set of multiple
$^{14}$C samples, $$X_1, \ldots, X_n$$ the function `PPcalibrate` will
simultaneously:

- calibrate them to provide posterior calendar ages estimates
  $\theta_1, \ldots, \theta_n$
- estimate the rate $\lambda(t)$ of the underlying process. This
  estimate $\hat{\lambda}(t)$ may provide a useful proxy to make
  archaeological and/or environmental inference, e.g., to model changes
  in culture, or useage, or population size over time.

The (calendar) rate at which the samples occur is modelled as a Poisson
process with a variable rate $\lambda(t)$. Specifically, $\lambda(t)$ is
modelled as a piecewise constant function with an unknown number of
changepoints: $$
\lambda(t) = \left\{ \begin{array}{ll} 
  h_i \quad & \textrm{if } s_i \leq t < s_{i+1} \\
  0 & \textrm{otherwise}
  \end{array} \right.
$$ where $T_A < s_1 < s_2 < \ldots < s_k < T_B$ are the calendar ages at
which there are changes in the Poisson process rate, and $T_A$ and $T_B$
are bounding calendar ages. We use reversible jump MCMC (RJMCMC) to
alternate between calibrating the samples and updating the estimate of
the underlying rate lambda.

## Installation

You can install the development version of RJradiocarbon from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("TJHeaton/RJradiocarbon")
```

## Example

This is a basic example which shows you how to solve a common problem.
In this simulated example, we will create 40 artificial samples with
$^{14}$C determinations corresponding to the calendar period from \[550,
500\] cal yr BP. Effectively, the underlying model is a Poisson process
with a rate $$
\lambda(t)  \left\{ \begin{array}{ll}
  h_1 = 0 & \textrm{if } 400 \leq t < 500 \textrm{ cal yr BP} \\
  h_2 \qquad & \textrm{if } 500 \leq t < 550 \textrm{ cal yr BP}\\
  h_3 = 0 & \textrm{if } 550 \leq t < 700 \textrm{ cal yr BP} 
  \end{array} \right.
$$ We will then investigate if we can reconstruct this underlying
information, from just the $^{14}$C values, if we only knew that the
calendar ages of the samples lay between the bounding \[700,400\] cal yr
BP.

``` r
library(RJradiocarbon)
## basic example code
set.seed(15)
library(carbondate)

# Set initial values 
n_observed <- 40
n_iter <- 100000
n_thin <- 10
rc_sigmas <- rep(15, n_observed)
calendar_grid_resolution <- 1

# Create artificial rc_determinations
calendar_age_range <- c(400, 700) 
observed_age_range <- c(500, 550) 
true_theta <- seq(from = observed_age_range[1],
                  to = observed_age_range[2],
                  length = n_observed)
intcal_mean <- approx(x = intcal20$calendar_age_BP,
                      y = intcal20$c14_age,
                      xout = true_theta)$y
intcal_sd <- approx(x = intcal20$calendar_age_BP,
                    y = intcal20$c14_sig,
                    xout = true_theta)$y
rc_determinations <- rnorm(n = n_observed,
                                  mean = intcal_mean,
                                  sd = sqrt(rc_sigmas^2 + intcal_sd^2))

# Fit the model
PP_fit_output <- PPcalibrate(
    rc_determinations = rc_determinations,
    rc_sigmas = rc_sigmas,
    calibration_curve = intcal20,
    calendar_age_range = calendar_age_range,
    calendar_grid_resolution = calendar_grid_resolution,
    n_iter = n_iter,
    n_thin = n_thin,
    show_progress = FALSE)
```

This creates an object `PP_fit_output` which you can access directly or
use the in-built plotting functions.

### Plotting the posterior mean estimate of $\lambda(t)$

To plot the posterior estimate of $\lambda(t)$ you should use the
in-built function:

``` r
PlotPosteriorMeanRate(PP_fit_output)
```

<img src="man/figures/README-meanrate-1.png" width="100%" /> This plots
the posterior mean rate of sample occurrence (shown in purple). The
$^{14}$C determinations ($X_1, \ldots, X_n$) are shown on the y-axis as
a rug. The rug on the x-axis plots the posterior mean of the
corresponding calendar ages (i.e., $\theta_1, \ldots, \theta_n$) after
calibration for illustration purposes. **Note**: In some cases (in
particular when an individual calibrated age estimates $\theta_i$ is
bimodal) the posterior calendar mean will not be a good summary.

As you can see, the posterior estimate of $\lambda(t)$ identifies the
calendar period from which the hypothetical samples have been created
\[550, 500\] cal yr BP. Here, $\hat{\lambda}(t)$ is non-zero, while for
the other time periods.

### Plotting the estimated number of changepoint

You can plot the posterior estimate for the number of internal changes
in the rate $\lambda(t)$ using the command:

``` r
PlotNumberOfInternalChanges(PP_fit_output)
```

<img src="man/figures/README-changepoint number-1.png" width="100%" />

Here, we can see that the method estimates that there are most likely to
be two changes. This is clearly an overestimate of the number of changes
— in our underlying data there are only two changes, a step up from a
occurrence rate of zero samples/cal yr to a rate of *ca.* 0.8
samples/cal yr at 550 cal yr BP, and then a step back down to return to
a sample occurrence rate of zero at 500 cal yr BP.

This overestimation is largely a consequence of the prior we have chosen
for the number of internal changes in the rate $\lambda(t)$. This has a
default of 10 in `PPcalibrate()` to enable more flexibility in rate
estimation when the true rate is not piecewise constant. The number of
estimated changes will be pulled towards this value.

### Plotting the estimates of changepoint locations

You can plot the locations of the changepoints, conditional on the
number of changes currently estimated in the model. The default shows
the locations when the model estimates
$n_{\textrm{internal changes}} = 1, 2,$ and 3.

``` r
PlotPosteriorChangePoints(PP_fit_output)
#> Warning in PlotPosteriorChangePoints(PP_fit_output): No posterior samples with 1
#> internal changes
```

<img src="man/figures/README-changepoint locations-1.png" width="100%" />

This figure can be interpreted as follows. For example, if the model
thinks there are only two changepoints in the underlying occurrence rate
of the samples, then these changepoints should be located around 500 cal
yr BP and 550 cal yr BP (the two distinct dashed density estimates shown
in dark green). If the model thinks there are three changepoints in the
rate, then these should be located according to the location shown by
the dotted red densities (note that here these three locations overlay
one another, effectively adding a double changepoint at either 500 or
550 cal yr BP). Here, the model never thinks there should be only one
changepoint (as indicated by the absence of a solid blue density, and
the warning message).

### Plotting the rate estimates conditional on the number of changes

Finally, you can plot the estimates of the rate of sample occurrence
conditional on the number of changes estimate

``` r
PlotPosteriorHeights(PP_fit_output)
#> Warning in PlotPosteriorHeights(PP_fit_output): No posterior samples with 1
#> internal changes
```

<img src="man/figures/README-changepoint rates-1.png" width="100%" />

Here, conditional on there being two changes in $\lambda(t)$ between
700–400 cal yr BP (so three different values for the rates) we have two
rates of *ca.* 0 events per cal yr (these correspond to the periods
700–550 and 500–400 cal yr BP), and a period with rate *ca.* 0.6-0.8
events per cal yr (corresponding to the period from 550–500 cal yr BP)
