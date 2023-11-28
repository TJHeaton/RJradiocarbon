.FindIntegral <- function(rate_s, rate_h) {
  nchangepoints <- length(rate_s)
  nheights <- length(rate_h)

  if(nheights != (nchangepoints - 1)) stop("Incompatible information on rate s and h")

  sum(rate_h * diff(rate_s))
}



# Need care with using the sample command as sometimes we pass a single integer j.
# If use sample() then will draw from 1:j which is not what we want
# This resample function will stop this happening
.resample <- function(x, size, ...)
{
  if(length(x) <= 1) {
    if(!missing(size) && size == 0) x[FALSE]
    else x
  }
  else sample(x, size, ...)
}

