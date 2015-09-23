library(MASS)

# Parameters
kPhi <- 4
kVariance <- 4
kMean <- 10
grid.length <- 10

# kColours <- terrain.colors
kColours <- heat.colors

# Random observations

# # Set up matrices for x and y
# number.of.observations <- 15
# 
# sample.x <-
#   sort(sample(1:grid.length, number.of.observations, replace = T))
# sample.y <-
#   sample(1:grid.length, number.of.observations, replace = T)
# 
# while (T) {
#   duplicate <- F
#   for (i in 2:number.of.observations) {
#     if (sample.x[i - 1] == sample.y[i - 1] && sample.x[i] == sample.y[i]) {
#       duplicate <- T
#       break
#     }
#   }
#   if (!duplicate) {
#     break;
#   }
#   sample.x <-
#     sort(sample(1:grid.length, number.of.observations, replace = T))
#   sample.y <-
#     sample(1:grid.length, number.of.observations, replace = T)
# }
# 
# y.coords <- cbind(sample.x, sample.y)
# observations <- matrix(rep(5, number.of.observations), nrow = number.of.observations, ncol = 1)
# y.avg <- mean(observations)
# 
# x.number.of.points <- grid.length ^ 2 - number.of.observations


# Observations in a rectangle
number.of.observations <- 4
y.coords = matrix(nrow = number.of.observations, ncol = 2)
y.coords[1, 1] <- 3
y.coords[1, 2] <- 5
y.coords[2, 1] <- 3
y.coords[2, 2] <- 7
y.coords[3, 1] <- 5
y.coords[3, 2] <- 5
y.coords[4, 1] <- 5
y.coords[4, 2] <- 7
observations <- matrix(c(5, 7, 15, 15), nrow = number.of.observations, ncol = 1)
y.avg <- mean(observations)


x.number.of.points <- grid.length ^ 2 - number.of.observations

source('helper_functions.R')



# Choose some points where there are observations
# y.coords = matrix(nrow = 3, ncol = 2)
#
# y.coords[1, 1] <- 3
# y.coords[1, 2] <- 5
# y.coords[2, 1] <- 3
# y.coords[2, 2] <- 7
# y.coords[3, 1] <- 5
# y.coords[3, 2] <- 7
#
# observations <- matrix(c(20, 5, 15), nrow = 3, ncol = 1)
#
# y.avg <- mean(observations)






IsPointObservation <- function(i, j) {
  for (k in 1:number.of.observations) {
    if (y.coords[k, 1] == i && y.coords[k, 2] == j) {
      return(T)
    }
  }
  return(F)
}


x.coords <- matrix(NA, nrow = x.number.of.points, ncol = 2)
point.number <- 1
for (i in 1:grid.length) {
  for (j in 1:grid.length) {
    if (!IsPointObservation(i, j)) {
      x.coords[point.number, 1] <- i
      x.coords[point.number, 2] <- j
      point.number <- point.number + 1
    }
  }
}

field.data <- list(x.coords = x.coords, y.coords = y.coords, number.of.observations = number.of.observations,
                   y.avg = y.avg, observations = observations)

source('x_given_y_avg_v2.R')
# source('x_given_y_exact.R')
# source('abc_gaussian_field_v2.R')
