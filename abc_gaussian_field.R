library(MASS)
library(geoR)

# Parameters
kPhi <- 5
kVariance <- 4
kMean <- 10
kDistance.between.gridlines <- 10
grid.length <- 5


CovarianceFunction <- function(distance) {
  kVariance * exp(-distance / kPhi)
}

CalculateCovariance <-
  function(x1.coord, y1.coord, x2.coord, y2.coord, DistanceCovarFunc)  {
    CovarianceFunction(sqrt((x1.coord - y1.coord) ^ 2 + (x2.coord - y2.coord) ^
                              2))
  }

mu.x <- matrix(rep(kMean, grid.length ^ 2), ncol = 1)
cov.mat.x <-
  toeplitz(sapply(1:grid.length ^ 2, function(d) {
    CovarianceFunction(d)
  }))


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
  })
}), nrow = obs.length, byrow = T)


# For the prediction points the coordinates are
# the indices times kDistance.between.gridlines
pred.coords <- matrix(sapply(1:grid.length, function(x) {
  sapply(1:grid.length, function(y) {
    c(x * kDistance.between.gridlines, y * kDistance.between.gridlines)
  })
}), ncol = 2, byrow = T)

cov.mat.y.x <- sapply(1:obs.length, function(obs.index) {
  sapply(1:grid.length ^ 2, function(pred.index) {
    CalculateCovariance(obs.coords[obs.index, 1], pred.coords[pred.index, 1], obs.coords[obs.index, 2], pred.coords[pred.index, 2])
  })
})



# Compute inverses
cov.mat.x.inv <- solve(cov.mat.x)
cov.mat.y.inv <- solve(cov.mat.y)

x.proposal <-
  matrix(
    mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length ^ 2, ncol = 1, byrow = T
  )
# Set up expressions for y given x
mu.y.given.x <-
  mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (x.sample - mu.x)
cov.mat.y.given.x <-
  cov.mat.y - t(cov.mat.y.x) %*% cov.mat.x.inv %*% cov.mat.y.x

# Now the parameters for the distribution to y average given x
mu.y.avg <- mean(mu.y.given.x)
A <- matrix(1 / obs.length, nrow = 1, ncol = obs.length)
var.y.avg <- A %*% cov.mat.y.given.x %*% t(A)

#sample.y.avg <- rnorm(n = 1, mean = mu.y.avg, sd = sqrt(var.y.avg))

y.test.avg <- 10

# pnorm(y.test.avg, mean = mu.y.avg, sd = sqrt(var.y.avg))

MultivariateGaussian <-
  function(my.x, my.mu, my.sigma, my.sigma.inv) {
    (2 * pi) ^ -(length(my.mu) / 2) * sqrt((det(my.sigma))) * exp(-(1 / 2) * t(my.x - my.mu) %*% my.sigma.inv %*% (my.x - my.mu))
  }

RandomWalkMetropolisHastingsMCMC <- function(previous.sample) {
  # Using the prior as the proposal distribution
  x.proposal <-
    matrix(
      mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length ^ 2, ncol = 1, byrow = T
    )
  
  # print(MultivariateGaussian(x.proposal, mu.x, cov.mat.x, cov.mat.x.inv))
  
  old.my.mu.y.given.x <-
    mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (previous.sample - mu.x)
  my.mu.y.given.x <-
    mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (x.proposal - mu.x)
  
  old.y.avg.given.x <-
    pnorm(y.test.avg, mean = mean(old.my.mu.y.given.x), sd = sqrt(var.y.avg))
  new.y.avg.given.x <-
    pnorm(y.test.avg, mean = mean(my.mu.y.given.x), sd = sqrt(var.y.avg))
  
  print(paste("old.y.avg.given.x: ", log(old.y.avg.given.x)))
  print(paste("new.y.avg.given.x: ", log(new.y.avg.given.x)))
  print(paste("new: ", log(
    MultivariateGaussian(x.proposal, mu.x, cov.mat.x, cov.mat.x.inv)
  )))
  print(paste("old: ", log(
    MultivariateGaussian(previous.sample, mu.x, cov.mat.x, cov.mat.x.inv)
  )))
  
  # Metropolis-Hastings ratio
  log.ratio <-
    log(MultivariateGaussian(x.proposal, mu.x, cov.mat.x, cov.mat.x.inv)) + log(new.y.avg.given.x) - log(MultivariateGaussian(previous.sample, mu.x, cov.mat.x, cov.mat.x.inv)) - log(old.y.avg.given.x)
  
  print(paste('log ratio: ', log.ratio))
  print(exp(log.ratio))
  
  if (runif(1) < min(1, exp(log.ratio))) {
    print('Accepting')
    return(x.proposal)
  }
  else {
    return(previous.sample)
  }
}

number.of.iterations <- 5000
samples <- vector('list', number.of.iterations)
sample.probabilities <- vector('list', number.of.iterations)
sample.acceptance <- vector('list', number.of.iterations)

x.current <-
  matrix(
    mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length ^ 2, ncol = 1, byrow = T
  )
for (i in 1:number.of.iterations) {
  # print(i)
  # print(MultivariateGaussian(x.current, mu.x, cov.mat.x, cov.mat.x.inv))
  x.new <- RandomWalkMetropolisHastingsMCMC(x.current)
  
  if (all(x.new == x.current)) {
    sample.acceptance[[i]] <- 0
  }
  else {
    sample.acceptance[[i]] <- 1
  }
  x.current <- x.new
  
  samples[[i]] <- x.current
  sample.probabilities[[i]] <-
    MultivariateGaussian(x.current, mu.x, cov.mat.x, cov.mat.x.inv)
}

# sample.indices <- c(1:number.of.iterations)%%50 == 0
keep.samples <- seq(from = 1, to = number.of.iterations, by = 50)

sub.samples <- vector('list', length(keep.samples))

for (i in 1:length(keep.samples)) {
  sub.samples[[i]] <- samples[[keep.samples[i]]]
}

# samples.mean <- sapply(1:grid.length, function(x) {
#   sapply(1:grid.length, function(y) {
#     (1 / length(sub.samples)) * sum(sapply(sub.samples, function(sub.sample) {
#       sub.sample[(x - 1) * grid.length + 1]
#     }))
#   })
# })

samples.mean <-
  (1 / length(sub.samples)) * sapply(1:grid.length ^ 2, function(x) {
    sum(sapply(sub.samples, function(sub.sample) {
      sub.sample[[x]]
    }))
  })

samples.mean.matrix <-
  matrix(samples.mean, nrow = grid.length, ncol = grid.length, byrow = T)




# ABC approach
kTolerance <- 1
StatisticDistanceFunction <-
  function(proposed.sample.statistic, observed.statistic) {
    (proposed.sample.statistic - observed.statistic) ^ 2
  }

number.of.abc.samples <- 100
abc.samples <- vector('list', number.of.abc.samples)
abc.prior.avg <- mean(abc.prior)
counter <- 1

while (counter <= length(abc.samples)) {
  abc.prior <- mvrnorm(mu = mu.x, Sigma = cov.mat.x)
  if (StatisticDistanceFunction(abc.prior.avg, y.test.avg) < kTolerance) {
    abc.samples[[counter]] <- abc.prior
    counter <- counter + 1
  }
}









# png ( " problem1b _ posterior _ expected _ value . png " )
# posterior . expected . value <- sapply (1: length . seismic , function ( y )
# { sapply (1: length . seismic , function ( x )
# {(1 / number . of . samples ) * sum ( sapply (1: number . of . samples , function ( z )
# { sample . images [[ z ]][ x , y ]}) ) }) })
# filled . contour ( posterior . expected . value )
# dev . off ()



# cond.mean <- matrix(data = mu2, nrow = grid.length, ncol = grid.length)
# filled.contour(1:grid.length, 1:grid.length, matrix(random.sample, nrow = grid.length, ncol = grid.length), color = heat.colors)
# filled.contour(1:grid.length, 1:grid.length, cond.mean, color = heat.colors)
