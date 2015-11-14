# Evenly spaced observations
obs.x.coords <- seq(obs.x.start, obs.x.stop, obs.x.space)
obs.y.coords <- seq(obs.y.start, obs.y.stop, obs.y.space)

y.coords <- matrix(NA, nrow = length(obs.x.coords) * length(obs.y.coords), ncol = 2)
y.coords[ , 1] <- rep(obs.x.coords, length(obs.y.coords))
y.coords[ , 2] <- as.vector(matrix(obs.y.coords, nrow = length(obs.y.coords), ncol = length(obs.y.coords), byrow = T))

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


observations <- matrix(sapply(1:dim(y.coords)[1], function(x) {
  actual.structure[y.coords[x, 1], y.coords[x, 2]]
}),  nrow = dim(y.coords)[1], ncol = 1)
y.avg <- mean(observations)



D <- matrix(0, nrow = dim(y.coords)[1], ncol = grid.length^2)
for(i in 1:dim(y.coords)[1]) {
  D[i, (y.coords[i, 2] - 1) * grid.length + y.coords[i, 1]] <- 1
}
