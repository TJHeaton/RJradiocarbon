#' Title
#'
#' @param theta Vector of the observed times of the events
#' @param rate_s The current changepoints in the rate
#' @param rate_h The current heights in the rate (corresponding to above)
#' @param integrated_rate The integral of piecewise constant rate i.e.,
#' integral_0^L nu(t) dt
#' @param prior_h_shape,prior_h_rate prior parameters on heights
#' We assume each height in piecewise rate has h ~ Gamma(shape, rate)

#' @param prior_n_change_lambda Prior on number of internal changepoints n ~ Po(lambda)
#' @param prob_move Dataframe with probability of each type of RJ move
#' (change_pos, change_height, birth, death)
#'
#' @return TO DO
#' @export
#'
#' @examples # TO DO
RevJumpUpdatePoissonProcessRate <- function(
    theta,
    rate_s,
    rate_h,
    integrated_rate,
    prior_h_shape,
    prior_h_rate,
    prior_n_change_lambda,
    prob_move) {

  n_changepoints <- length(rate_s)
  n_heights <- length(rate_h)
  n_internal_changepoints <- n_changepoints - 2

  # TO DO - CHECK ARGUMENTS OF prob_move

  if(n_heights != n_changepoints - 1) {
    stop("Error in matching dimension of rate_s and rate_h")
  }

  u <- runif(1)

  if(u < prob_move$pos[n_heights]) # Propose moving position of a changepoint
  {
    update <- .ChangePos(
      theta = theta,
      rate_s = rate_s,
      rate_h = rate_h,
      integrated_rate = integrated_rate)
    rate_s <- update$rate_s
    integrated_rate <- updte$integrated_rate
  }
  else if(u < (
    prob_move$pos[n_heights]
    + prob_move$height[n_heights]
    )) # Propose moving height of a step
  {
    update <- .ChangeHeight(
      theta = theta,
      rate_s = rate_s,
      rate_h = rate_h,
      integrated_rate = integrated_rate,
      prior_h_shape = prior_h_shape,
      prior_h_rate = prior_h_rate)
    rate_h <- update$rate_h
    integrated_rate <- update$integrated_rate
  }
  else if(u < (
    prob_move$pos[n_heights]
    + prob_move$height[n_heights]
    + prob_move$birth[n_heights]
    )) # Propose birth step
  {
    proposal_ratio <- (
      (prob_move$death[n_heights + 1] * (rate_s[n_changepoints] - rate_s[1]))
      / (prob_move$birth[n_heights] * (n_internal_changepoints + 1))
    )
    update <- .Birth(
      theta = theta,
      rate_s = rate_s,
      rate_h = rate_h,
      integrated_rate = integrated_rate,
      prior_h_shape =  prior_h_shape,
      prior_h_rate = prior_h_rate,
      prior_n_change_lambda = prior_n_change_lambda,
      proposal_ratio = proposal_ratio)
    rate_s <- update$rate_s
    rate_h <- update$rate_h
    integrated_rate <- update$integrated_rate
  }
  else # Propose a death step
  {
    proposal_ratio <- (
      (prob_move$birth[n_heights - 1 ] * n_internal_changepoints)
      / (prob_move$death[n_heights] * (rate_s[n_changepoints] - rate_s[1]))
    )
    update <- .Death(
      theta = theta,
      rate_s = rate_s,
      rate_h = rate_h,
      integrated_rate = integrated_rate,
      prior_h_shape = prior_h_shape,
      prior_h_rate = prior_h_rate,
      prior_n_change_lambda = prior_n_change_lambda,
      proposal_ratio = proposal_ratio)

    rate_s <- update$rate_s
    rate_h <- update$rate_h
    integrated_rate <- update$integrated_rate
  }

  list(rate_s = rate_s, rate_h = rate_h, integrated_rate = integrated_rate)
}
