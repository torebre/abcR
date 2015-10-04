library(MASS)
library(latex2exp)


# Parameters
kPhi <- 25
kVariance <- 4
kMean <- 10
kObsNoiseVar <- 2
grid.length <- 10
number.of.observations <- 4

# kColours <- terrain.colors
kColours <- heat.colors

source('helper_functions.R')

source('setup_observations.R')

source('x_given_y_avg_v2.R')

source('x_given_y_exact.R')

source('abc_gaussian_field_v2.R')
