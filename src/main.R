library(MASS)
library(ggplot2)

# Parameters
kPhi <- 25
kVariance <- 4
kMean <- 10
kObsNoiseVar <- 5
grid.length <- 100
number.of.observations <- 100

# kColours <- terrain.colors
kColours <- heat.colors

source('helper_functions.R')

source('setup_observations.R')

source('x_given_y_avg_v2.R')

# source('x_given_y_exact.R')
# source('abc_gaussian_field_v2.R')
