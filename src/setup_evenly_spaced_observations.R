# Evenly spaced observations
y.coords = matrix(nrow = number.of.observations, ncol = 2)

obs.x.coords <- seq(obs.x.start, obs.x.stop, obs.x.space)
obs.y.coords <- seq(obs.y.start, obs.y.stop, obs.y.space)

y.coords <- matrix(NA, nrow = length(obs.x.coords) * length(obs.y.coords), ncol = 2)
y.coords[ , 1] <- rep(obs.x.coords, length(obs.y.coords))
y.coords[ , 2] <- as.vector(matrix(obs.y.coords, nrow = length(obs.y.coords), ncol = length(obs.y.coords), byrow = T))
# seq(rep(obs.y.coords, length(obs.y.coords)))
  
#   sapply(seq(obs.y.start, obs.y.stop, obs.y.space), function(y) {
#   obs.row <- matrix(NA, nrow = number.of.observations, ncol = 2)
#   obs.row[ , 1] <- obs.x.coords
#   obs.row[ , 2] <- rep(y, number.of.observations)
#   # return obs.row
# })



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

