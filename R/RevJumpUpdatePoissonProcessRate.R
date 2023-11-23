#' Title
#'
#' @param theta Vector of the observed times of the events
#' @param rate_s The current changepoints in the rate
#' @param rate_h The current heights in the rate (corresponding to above)
#' @param integrated_rate The integrak of piecewise constant rate i.e.,
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

  # Empty function
  print("Hello!")

}
