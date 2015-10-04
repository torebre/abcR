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
# number.of.observations <- 4
# y.coords = matrix(nrow = number.of.observations, ncol = 2)
# y.coords[1, 1] <- 3
# y.coords[1, 2] <- 5
# y.coords[2, 1] <- 3
# y.coords[2, 2] <- 7
# y.coords[3, 1] <- 5
# y.coords[3, 2] <- 5
# y.coords[4, 1] <- 5
# y.coords[4, 2] <- 7
# observations <- matrix(c(9, 9, 9, 9), nrow = number.of.observations, ncol = 1)
# y.avg <- mean(observations)


# # Observations spread out
# number.of.observations <- 4
# y.coords = matrix(nrow = number.of.observations, ncol = 2)
# y.coords[1, 1] <- 1
# y.coords[1, 2] <- 1
# y.coords[2, 1] <- 10
# y.coords[2, 2] <- 10
# y.coords[3, 1] <- 6
# y.coords[3, 2] <- 2
# y.coords[4, 1] <- 7
# y.coords[4, 2] <- 2
# observations <- matrix(c(7, 8, 12, 9), nrow = number.of.observations, ncol = 1)
# y.avg <- mean(observations)



y.coords = matrix(nrow = number.of.observations, ncol = 2)

# Assign random coordinates for observations
sample.x <-
  sort(sample(1:grid.length, number.of.observations, replace = T))
sample.y <-
  sample(1:grid.length, number.of.observations, replace = T)

# Check that no two observations are on top of each other
while (T) {
  duplicate <- F
  for (i in 2:number.of.observations) {
    if (sample.x[i - 1] == sample.y[i - 1] && sample.x[i] == sample.y[i]) {
      sample.y[i] <- sample(1:grid.length, 1)
      duplicate <- T
      break
    }
  }
  if (!duplicate) {
    break;
  }
}

y.coords <- cbind(sample.x, sample.y)


actual.cov.mat <- matrix(sapply(1:grid.length, function(x1) {
  sapply(1:grid.length, function(y1) {
    sapply(1:grid.length, function(x2) {
      sapply(1:grid.length, function(y2) {
        CalculateCovariance(x1, x2, y1, y2)    
      })
    })
  })
}), nrow = grid.length^2, ncol = grid.length^2, byrow = T)

actual.structure <- matrix(mvrnorm(mu = rep(kMean, grid.length^2), Sigma = actual.cov.mat), nrow = grid.length, ncol = grid.length)





# observations <- matrix(rnorm(number.of.observations, mean = kMean, sd = sqrt(kObsNoiseVar)), nrow = number.of.observations, ncol = 1)

observations <- matrix(sapply(1:number.of.observations, function(x) {
  actual.structure[y.coords[x, 1], y.coords[x, 2]]
}),  nrow = number.of.observations, ncol = 1)
y.avg <- mean(observations)
x.number.of.points <- grid.length ^ 2 - number.of.observations

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

