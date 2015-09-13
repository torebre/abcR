library(MASS)
library(geoR)

# Parameters
kPhi <- 5
kVariance <- 4
kMean <- 10
kDistance.between.gridlines <- 10
grid.length <- 30


CovarianceFunction <- function(distance) {
  kVariance * exp(-distance / kPhi)
}

CalculateCovariance <- function(x1.coord, y1.coord, x2.coord, y2.coord, DistanceCovarFunc)  {
  CovarianceFunction(sqrt((x1.coord - y1.coord)^2 + (x2.coord - y2.coord)^2))
}

mu.x <- matrix(rep(kMean, grid.length^2), ncol = 1)
cov.mat.x <- toeplitz(sapply(1:grid.length^2, function(d) { CovarianceFunction(d)}))


# Create a single sample realisation
# simulation <- grf(n = grid.length^2, grid = expand.grid(1:grid.length, 1:grid.length), 
#                  cov.model = "exponential", cov.pars = c(kVariance, kPhi), mean = kMean)
# random.sample <- matrix(simulation$data, nrow = 1)

random.sample <- mvrnorm(n = 1, rep(kMean, grid.length^2), cov.mat)
image(matrix(random.sample, nrow = grid.length, ncol = grid.length))

# To avoid extra complexity, the observation points do not 
# overlap with the prediction points
obs.coords = matrix(nrow = 3, ncol = 2)
obs.coords[1, 1] <- 5
obs.coords[1, 2] <- 5
obs.coords[2, 1] <- 15
obs.coords[2, 2] <- 15
obs.coords[3, 1] <- 25
obs.coords[3, 2] <- 25

obs.length <- dim(obs.coords)[1]

mu.y <- matrix(rep(kMean, obs.length), ncol = 1)

cov.mat.y <- matrix(sapply(1:obs.length, function(x.index) {
  sapply(1:obs.length, function(y.index) { 
    CalculateCovariance(obs.coords[x.index, 1], obs.coords[y.index, 1], obs.coords[x.index, 1], obs.coords[y.index, 1])
  })}), nrow = obs.length, byrow = T)


# For the prediction points the coordinates are 
# the indices times kDistance.between.gridlines
pred.coords <- matrix(sapply(1:grid.length, function(x) {
  sapply(1:grid.length, function(y) {
    c(x * kDistance.between.gridlines, y * kDistance.between.gridlines)
  })
}), nrow = 900, ncol = 2, byrow = T)

cov.mat.y.x <- sapply(1:obs.length, function(obs.index) {
  sapply(1:grid.length^2, function(pred.index) {
    CalculateCovariance(obs.coords[obs.index, 1], pred.coords[pred.index, 1], obs.coords[obs.index, 2], pred.coords[pred.index, 2])
  })
})

x.sample <- matrix(mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length^2, ncol = 1, byrow = T)

cov.mat.x.inv <- solve(cov.mat.x)
cov.mat.y.inv <- solve(cov.mat.y)

# Set up expressions for y given x
mu.y.given.x <- mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (x.sample - mu.x)
cov.mat.y.given.x <- cov.mat.y - t(cov.mat.y.x) %*% cov.mat.x.inv %*% cov.mat.y.x

# Now the parameters for the distribution to y average given x
mu.y.avg <- kMean
A <- matrix(1/obs.length, nrow = 1, ncol = obs.length)
var.y.avg <- A %*% cov.mat.y.given.x %*% t(A)


sample.y.avg <- rnorm(n = 1, mean = mu.y.avg, sd = sqrt(var.y.avg))


cov.mat.x.inv <- solve(cov.mat.x)
cov.mat.y.inv <- solve(cov.mat.y)



# # Create mean vector and covariance matrix based on the calculations 
# # done previously in the problem
# B1 = matrix(0, nrow = length(observations.indices), ncol = length(random.sample))
# for(i in 1:length(observations.indices)) {
#   B1[i, observations.indices] <- 1
# }




sigma22fInv = solve(diag(length(observations.indices)) + B1 %*% cov.mat %*% t(B1))
mu2 = rep(kMean, length(random.sample)) + t(cov.mat) %*% t(B1) %*% sigma22fInv %*% (obs.with.noise - kMean)
cov.mat.2 = cov.mat - t(cov.mat) %*% t(B1) %*% sigma22fInv %*% B1 %*% cov.mat


cond.mean <- matrix(data = mu2, nrow = grid.length, ncol = grid.length)

filled.contour(1:grid.length, 1:grid.length, matrix(random.sample, nrow = grid.length, ncol = grid.length), color = heat.colors)

filled.contour(1:grid.length, 1:grid.length, cond.mean, color = heat.colors)


