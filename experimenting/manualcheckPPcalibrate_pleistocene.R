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
prior_n_internal_changepoints_lambda <- 5
k_max_internal_changepoints <- 30
rescale_factor_rev_jump <- 0.9
default_prior_h_rate <- 0.1
initial_n_internal_changepoints <- 10

n_iter <- 50000
n_thin <- 10
F14C_inputs <- FALSE
use_F14C_space <- TRUE

calibration_curve <- intcal20
calendar_grid_resolution <- 10
show_progress <- TRUE

set.seed(14)
# set.seed(19)

Test_Output <- PPcalibrate(
  rc_determinations = rc_determinations,
  rc_sigmas = rc_sigmas,
  calibration_curve = calibration_curve,
  F14C_inputs = F14C_inputs,
  n_iter = n_iter,
  n_thin = n_thin,
  use_F14C_space = use_F14C_space,
  show_progress = show_progress,
  calendar_grid_resolution = calendar_grid_resolution,
  prior_n_internal_changepoints_lambda = prior_n_internal_changepoints_lambda,
  k_max_internal_changepoints = k_max_internal_changepoints,
  rescale_factor_rev_jump = rescale_factor_rev_jump,
  default_prior_h_rate = default_prior_h_rate,
  initial_n_internal_changepoints = initial_n_internal_changepoints
  )

###########################################################
### You have already done this
hist(Test_Output$n_internal_changes)
min_cal_age <- min(Test_Output$rate_s[[1]])
max_cal_age <- max(Test_Output$rate_s[[1]])
n_out <- length(Test_Output$n_internal_changes)
n_burn <- floor(n_out/2)
t_star <- seq(min_cal_age, max_cal_age, by = 5)

set.seed(25)
n_curves <- 2000
indices <- sample(
  n_burn:n_out,
  n_curves,
  replace = ((n_out - n_burn) < n_curves))
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
####################################################


###########################################
### TODO for Sara

# Plot 1 to show where the changepoints occur conditional on number

# Select vector to specify which n_internal_changes you want to add to the plot
which_n_changes_plot <- c(1,2,3) # Mimics Green paper

for(n_change in n_changes_plot) {
  index <- which(Test_Output$n_internal_changes == n_change)

  # Do nothing if no posterior samples with this number of changepoints
  if(length(index) == 0) continue

  # Otherwise extract the rates
  for(j in (1 + 1:n_change)) {
    # 1) Extract all posteriors with correct internal changes
    Test_Output$rate_s[index]
    # 2) Pull out jth value in the ordered list of changepoints
    # 3) Plot kde of the densoty of jth value
    # Choose plotting lty (and col) to vary according to nchanges

    # Note that we do not have any examples with n_internal small

    smoothed_density <- stats::density(Test_Output$rate_s[index][j])

      [j], bw = "SJ")
    graphics::lines(smoothed_density,
                    lwd = n_change,
                    col = "blue")
  }
}

### Plot 2 - Do the same but with rate_h
## Perhaps harmonise colours and lty (so comparable between Plot 1 and 2),
## i.e. determine based on n_changes

## Link to paper is https://people.maths.bris.ac.uk/~mapjg/papers/RJMCMCBka.pdf
## This will give Fig 3 and Fig 4
## It will get confusing for large numbers of changes
## (so perhaps note that tells people not to choose more than 3/4)












rm(true_theta, n_observed)
