## The 4 proposal reversible jump steps
## 1: Change the position h of a changepoint
## 2: Change the height s of a section
## 3: Remove a changepoint
## 4: Add a changepoint

## Proposal 1: Alter the position of a randomly chosen internal changepoint
# Arguments:
# calendar_ages - the observed thetas
# rate_s - the current changepoints in the rate
# rate_h - the heights
# integrated_rate - integral_0^L nu(t) dt
.ChangePos <- function(calendar_ages,
                       rate_s,
                       rate_h,
                       integrated_rate)
{
  n_changepoints <- length(rate_s)
  n_observations <- length(calendar_ages)

  if(n_changepoints < 3) stop("Internal Error (ChangePos): Proposed to move an internal changepoint when there are none")

  # Select internal changepoint to move at random
  j <- .resample(2:(n_changepoints-1), 1)
  # Special sample function as may pass single integer to pick from
  # [base function sample() would then pick from 1:j which is not correct]

  # Propose new changepoint position
  rate_s_new <- rate_s
  rate_s_new[j] <- runif(1, min = rate_s[j - 1], max = rate_s[j + 1])

  # Find prior ratio for rate s
  prior_rate_s_new   <- ((rate_s_new[j + 1] - rate_s_new[j])
                             * (rate_s_new[j] - rate_s_new[j - 1]))
  prior_rate_s_old <- ((rate_s[j + 1] - rate_s[j])
                             * (rate_s[j] - rate_s[j - 1]))

  prior_rate_s_ratio <- prior_rate_s_new / prior_rate_s_old

  # Find the likelihood of the theta data given both sets of changepoints (only changes in period)

  # Find new integrated_rate by adjusting previous integrated_rate
  # Calculates as (Diff in changepoint location) * (Diff in height)
  adjust_integral <- (rate_h[j] - rate_h[j - 1]) * (rate_s[j] - rate_s_new[j])
  integrated_rate_new <- integrated_rate + adjust_integral

  # Find which calendar_ages will contribute to the likelihood ratio and find their log-likelihood
  if(rate_s_new[j] < rate_s[j]) { # Have shifted new changepoint towards t = 0
    min_sj <- rate_s_new[j]
    max_sj <- rate_s[j]
    # Number of calendar ages that have been affected by changepoint shift
    n_calendar_ages_affected <- sum(calendar_ages > min_sj & calendar_ages < max_sj)

    # With old/current rate these calendar_ages have rate h[j-1] as before s[j]
    log_lik_old <- (n_calendar_ages_affected * log(rate_h[j-1])) - integrated_rate
    # With new rate they have rate h[j] as after s_new[j]
    log_lik_new <- (n_calendar_ages_affected * log(rate_h[j])) - integrated_rate_new

  } else { # Have shifted new changepoint away from t = 0
    min_sj <- rate_s[j]
    max_sj <- rate_s_new[j]
    # Number of calendar ages that have been affected by changepoint shift
    n_calendar_ages_affected <- sum(calendar_ages > min_sj & calendar_ages < max_sj)

    # With old/current rate these calendar_ages have rate h[j] as after s[j]
    log_lik_old <- (n_calendar_ages_affected * log(rate_h[j])) - integrated_rate
    # With new rate they have rate h[j-1] as before s_new[j]
    log_lik_new <- (n_calendar_ages_affected * log(rate_h[j-1])) - integrated_rate_new
  }

  calendar_ages_lik_ratio <- exp(log_lik_new - log_lik_old)

  # Find acceptance probability
  hastings_ratio <- calendar_ages_lik_ratio * prior_rate_s_ratio

  # Determine acceptance and return result
  if(runif(1) < hastings_ratio) {
    # Accept and return modified changepoints
    retlist <- list(s = rate_s_new, integrated_rate = integrated_rate_new)
  } else {
    # Else reject and return old changepoints
    retlist <- list(s = rate_s, integrated_rate = integrated_rate)
  }
  return(retlist)
}



# Small function which performs sampling as we want it to
# We need to take care with using the sample command as sometimes we pass a single integer j.
# If we use sample() then we will draw from 1:j which is not what we want
# This resample function will stop this happening
.resample <- function(x, size, ...)
{
  if(length(x) <= 1) {
    if(!missing(size) && size == 0) x[FALSE]
    else x
  }
  else sample(x, size, ...)
}

