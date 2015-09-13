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



# Compute inverses
cov.mat.x.inv <- solve(cov.mat.x)
cov.mat.y.inv <- solve(cov.mat.y)

# Set up expressions for y given x
mu.y.given.x <- mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (x.sample - mu.x)
cov.mat.y.given.x <- cov.mat.y - t(cov.mat.y.x) %*% cov.mat.x.inv %*% cov.mat.y.x

# Now the parameters for the distribution to y average given x
mu.y.avg <- mean(mu.y.given.x)
A <- matrix(1/obs.length, nrow = 1, ncol = obs.length)
var.y.avg <- A %*% cov.mat.y.given.x %*% t(A)

#sample.y.avg <- rnorm(n = 1, mean = mu.y.avg, sd = sqrt(var.y.avg))

y.test.avg <- 10



pnorm(y.test.avg, mean = mu.y.avg, sd = sqrt(var.y.avg))

MultivariateGaussian <- function(my.x, my.mu, my.sigma, my.sigma.inv) {
  (2 * pi)^-(length(my.mu) / 2) * sqrt((det(my.sigma))) * exp(-(1/2) * t((my.x - my.mu)) %*% my.sigma.inv %*% (my.x - my.mu))
}

RandomWalkMetropolisHastingsMCMC <- function(previous.sample) {
  # Using the prior as the proposal distribution
  x.proposal <- matrix(mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length^2, ncol = 1, byrow = T)  
  
  old.my.mu.y.given.x <- mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (previous.sample - mu.x)
  my.mu.y.given.x <- mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (x.proposal - mu.x)
  
  old.y.avg.given.x <- pnorm(y.test.avg, mean = old.my.mu.y.given.x, sd = sqrt(var.y.avg))
  new.y.avg.given.x <- pnorm(y.test.avg, mean = my.mu.y.avg, sd = sqrt(var.y.avg))
  
  # Metropolis-Hastings ratio
  log.ratio <- log(MultivariateGaussian(x.proposal, mu.x, cov.mat.x, cov.mat.x.inv) + new.y.avg.given.x 
                   - MultivariateGaussian(previous.sample, mu.x, cov.mat.x, cov.mat.x.inv) - old.y.avg.given.x)
  
  if(runif(1) < min(1, exp(log.ratio))) {
    return(x.proposal)
  }
  else {
    return(previous.sample)
  }
}


for(i in 1:100) {
  x.current <- matrix(mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length^2, ncol = 1, byrow = T)  
  x.current <- RandomWalkMetropolisHastingsMCMC <- function(x.current)
}


# cond.mean <- matrix(data = mu2, nrow = grid.length, ncol = grid.length)
# filled.contour(1:grid.length, 1:grid.length, matrix(random.sample, nrow = grid.length, ncol = grid.length), color = heat.colors)
# filled.contour(1:grid.length, 1:grid.length, cond.mean, color = heat.colors)


