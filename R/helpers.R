.FindIntegral <- function(rate_s, rate_h) {
  nchangepoints <- length(rate_s)
  nheights <- length(rate_h)

  if(nheights != (nchangepoints - 1)) stop("Incompatible information on rate s and h")

  sum(rate_h * diff(rate_s))
}


