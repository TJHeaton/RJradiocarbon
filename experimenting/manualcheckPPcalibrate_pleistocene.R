# An analysis of rates for various Pleistocene Animals from Guthrie
# https://www.nature.com/articles/nature04604#Sec3

# We assess RJ MCMC on rates for 14C dates for a range of animals
# We select 14C dates that lie between [6, 25] 14Cyrs BP

##############################################
Species <- "Bison" # Equus" or "Human" or "Mammoth" or "Bison" or "Alces" or "Cervus"

# Main function - you just enter the species and the calibration curve you want (interpolated onto a 5 yearly grid)
cutoffages <- c(6000, 25000)

# Read in the relevant datasets dependent upon species
if(Species == "Human") {
  data <- read.csv("experimenting/AnimalExtinctions/Humanc14Dates.csv", header = TRUE, na.strings = c("", "greater than"))
} else if(Species == "Equus") {
  # Need to force col classes as the spreadsheet has NA in it
  data <- read.csv("experimenting/AnimalExtinctions/Equusc14Dates.csv", header = TRUE, na.strings = c("", "greater than", "NA"),
                   colClasses = c("factor", "factor", "factor", "numeric", "numeric"))
} else if(Species == "Mammoth") {
  data <- read.csv("experimenting/AnimalExtinctions/Mammothc14Dates.csv", header = TRUE, na.strings = c("", "greater than"))
} else if(Species == "Alces") { # Moose/Elk
  data <- read.csv("experimenting/AnimalExtinctions/Alcesc14Dates.csv", header = TRUE, na.strings = c("", "greater than"))
} else if(Species == "Cervus") { # Dog
  data <- read.csv("experimenting/AnimalExtinctions/Cervusc14Dates.csv", header = TRUE, na.strings = c("", "greater than"))
} else if(Species == "Bison") { # Bison
  data <- read.csv("experimenting/AnimalExtinctions/Bisonc14Dates.csv", header = TRUE, na.strings = c("", "greater than"))
} else {stop("Unknown Species")}

keep <- which(data$X14C < cutoffages[2] & data$X14C > cutoffages[1])
data <- data[keep,]
removena <- which(is.na(data$X14C) | is.na(data$X1..Sigma))
if(length(removena) != 0) { # Remove any values where 14C or sigma is NA
  data <- data[-removena,]
}

rc_determinations <- data$X14C
rc_sigmas <- data$X1..Sigma

rm(keep, data, removena)

###### Used if pass to PPcalibrate
calendar_age_range <- c(0,50000)
rate_s <- NA
rate_h <- NA

######
prior_n_internal_changes_lambda <- 5
k_max_internal_changepoints <- 30
rescale_factor_rev_jump <- 0.9
n_iter <- 100000
n_thin <- 10
F14C_inputs <- FALSE
use_F14C_space <- FALSE
calibration_curve <- intcal20
calendar_grid_resolution <- 10

Test_Output <- PPcalibrate(
  rc_determinations = rc_determinations,
  rc_sigmas = rc_sigmas,
  calibration_curve = calibration_curve,
  prior_n_internal_changes_lambda = prior_n_internal_changes_lambda,
  prior_h_rate = 0.1,
  F14C_inputs = F14C_inputs,
  use_F14C_space = use_F14C_space,
  k_max_internal_changepoints = k_max_internal_changepoints,
  rescale_factor_rev_jump = rescale_factor_rev_jump,
  calendar_grid_resolution = calendar_grid_resolution,
  n_iter = n_iter,
  n_thin = n_thin)

hist(Test_Output$n_internal_changes)
min_cal_age <- min(Test_Output$rate_s[[1]])
max_cal_age <- max(Test_Output$rate_s[[1]])
n_out <- length(Test_Output$n_internal_changes)

t_star <- seq(min_cal_age, max_cal_age, by = 5)

set.seed(25)
n_curves <- 2000
indices <- sample(
  1:n_out,
  n_curves,
  replace = (n_out < n_curves))
rate <- matrix(NA, nrow = n_curves, ncol = length(t_star))

for(i in 1:n_curves) {
  ind <- indices[i]
  rate[i,] <- approx(
    x = Test_Output$rate_s[[ind]],
    y = c(Test_Output$rate_h[[ind]], 0),
    xout = t_star,
    method = "constant")$y
}

# Plot the poisson process rate
meanrate <- apply(rate, 2, mean)
CIrate <- apply(rate, 2, quantile, probs = c(0.025, 0.975))
CIrate <- apply(rate, 2, quantile, probs = c(1 - pnorm(1), pnorm(1)))
ymax <- 1.2 * max(CIrate)
plot(x = t_star,
     y = meanrate,
     ylim = c(0,ymax),
     xlim = c(max_cal_age, min_cal_age),
     type = "l",
     ylab = "Posterior Mean Rate",
     xlab = "Calendar Age (cal yr BP)" )
lines(t_star, CIrate[1,], col = "red", lty = 2)
lines(t_star, CIrate[2,], col = "red", lty = 2)
axis(1,
     at = seq(10000, 40000, by = 1000),
     labels = FALSE,
     lwd = 0.5,
     tck = -0.01)







rm(true_theta, n_observed)
