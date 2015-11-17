library(MASS)
library(latex2exp)

# Parameters
kPhi <- 10
kVariance <- 4
kMean <- 10
kObsNoiseVar <- 2
grid.length <- 25
# number.of.observations <- 4

obs.x.start <- 5
obs.x.space <- 5
obs.x.stop <- 20

obs.y.start <- 5
obs.y.space <- 5
obs.y.stop <-20

kColours <- heat.colors

source('helper_functions.R')

# source('setup_observations.R')

source('setup_evenly_spaced_observations.R')

# source('x_given_y_avg_v3.R')

# source('x_given_y_exact.R')

source('abc_gaussian_field_v2.R')
