library(MASS)
library(geoR)

# Parameters
kPhi <- 5
kVariance <- 4
kMean <- 10
kDistance.between.gridlines <- 4
grid.length <- 10

kColours <- terrain.colors

# Functions for calculating the covariance
CovarianceFunction <- function(distance) {
  print(distance)
  kVariance * exp(-distance / kPhi)
}

CalculateCovariance <-
  function(x1.coord, y1.coord, x2.coord, y2.coord)  {
    CovarianceFunction(sqrt((x1.coord - y1.coord)^2 + (x2.coord - y2.coord)^2))
  }

# Set up mean and covariance matrix for x
mu.x <- matrix(rep(kMean, grid.length ^ 2), ncol = 1)
cov.mat.x <- matrix(sapply(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), function(x1) {
  sapply(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), function(y1) {
    sapply(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), function(x2) {
      sapply(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), function(y2) {
        CalculateCovariance(x1, x2, y1, y2)
      })
    })
  })
}), nrow = grid.length^2, ncol = grid.length^2, byrow = T)
filled.contour(1:grid.length^2, 1:grid.length^2, cov.mat.x)

# Add some points were the values are known

# To avoid extra complexity, the observation points do not
# overlap with the prediction points
obs.coords = matrix(nrow = 3, ncol = 2)
obs.coords[1, 1] <- 3
obs.coords[1, 2] <- 5
obs.coords[2, 1] <- 21
obs.coords[2, 2] <- 21
obs.coords[3, 1] <- 21
obs.coords[3, 2] <- 37

obs.length <- dim(obs.coords)[1]

observations <- matrix(c(20, 1, 20), nrow = 3, ncol = 1)
# observations <- matrix(c(25), nrow = 1, ncol = 1)

# Set up mean and covariance matrix for y
mu.y <- matrix(rep(kMean, obs.length), ncol = 1)
cov.mat.y <- matrix(sapply(1:obs.length, function(x.index) {
  sapply(1:obs.length, function(y.index) {
    CalculateCovariance(obs.coords[x.index, 1], obs.coords[y.index, 1], obs.coords[x.index, 2], obs.coords[y.index, 2])
  })
}), nrow = obs.length, byrow = T)
image(cov.mat.y)
title('Covariance matrix y')
filled.contour(cov.mat.y)
title('Covariance matrix y')

# Compute the covariances between x and y
pred.coords <- matrix(sapply(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), function(x) {
  sapply(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), function(y) {
    c(x, y)
  })
}), ncol = 2, byrow = T)

cov.mat.y.x <- sapply(1:obs.length, function(obs.index) {
  sapply(1:grid.length ^ 2, function(pred.index) {
    CalculateCovariance(obs.coords[obs.index, 1], pred.coords[pred.index, 1], obs.coords[obs.index, 2], pred.coords[pred.index, 2])
  })
})

image(1:(grid.length^2), 1:length(observations), cov.mat.y.x)
title('Covariance between x and y')
filled.contour(cov.mat.y.x)
title('Covariance between x and y')

# Compute inverses
cov.mat.x.inv <- solve(cov.mat.x)
cov.mat.y.inv <- solve(cov.mat.y)

# Expressions for x given y
mu.x.given.y <- mu.x + cov.mat.y.x %*% cov.mat.y.inv %*% (observations - mu.y)
cov.mat.x.given.y <- cov.mat.x - cov.mat.y.x %*% cov.mat.y.inv %*% t(cov.mat.y.x)

samples.x.given.y <- mvrnorm(n = 50, mu = mu.x.given.y, Sigma = cov.mat.x.given.y)
mean.samples.x.given.y <- sapply(1:grid.length^2, function(x) {
  mean(samples.x.given.y[ , x])
})
mean.samples.x.given.y.matrix <- matrix(mean.samples.x.given.y, nrow = grid.length, ncol = grid.length, byrow = T)
var.samples.x.given.y.matrix <- matrix(sapply(1:grid.length^2, function(x) {
  var(samples.x.given.y[ , x])
}), nrow = grid.length, ncol = grid.length, byrow = T)

image(mean.samples.x.given.y.matrix)
title('Mean of samples from x given y"')
# Plot mean of samples showing x given y
filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               mean.samples.x.given.y.matrix, color = kColours, 
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title('Mean of samples from x given y')

filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               var.samples.x.given.y.matrix, color = kColours, 
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title('Variance x given y')

# Set up expressions for y given x
x.sample <- mvrnorm(n = 1, mu = mu.x, Sigma = cov.mat.x)
mu.y.given.x <-
  mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (x.sample - mu.x)
cov.mat.y.given.x <-
  cov.mat.y - t(cov.mat.y.x) %*% cov.mat.x.inv %*% cov.mat.y.x

# Now the parameters for the distribution to y average given x
mu.y.avg <- mean(mu.y.given.x)
A <- matrix(1 / obs.length, nrow = 1, ncol = obs.length)
var.y.avg <- A %*% cov.mat.y.given.x %*% t(A)

# Set the observed average to be the average of the observations
y.test.avg <- mean(observations)

# Evaluates a multivariate Gaussian
MultivariateGaussian <-
  function(my.x, my.mu, my.sigma, my.sigma.inv) {
    (2 * pi) ^ -(length(my.mu) / 2) * sqrt((det(my.sigma))) * exp(-(1 / 2) * t(my.x - my.mu) %*% my.sigma.inv %*% (my.x - my.mu))
  }

# Random walk Metropolis-Hastings MCMC function for getting samples 
# from x given y average
RandomWalkMetropolisHastingsMCMC <- function(previous.sample) {
  # Using the prior as the proposal distribution
  x.proposal <-
    matrix(
      mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length ^ 2, ncol = 1, byrow = T
    )
  
  old.my.mu.y.given.x <-
    mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (previous.sample - mu.x)
  my.mu.y.given.x <-
    mu.y + t(cov.mat.y.x) %*% cov.mat.x.inv %*% (x.proposal - mu.x)
  
  old.y.avg.given.x <-
    pnorm(y.test.avg, mean = mean(old.my.mu.y.given.x), sd = sqrt(var.y.avg))
  new.y.avg.given.x <-
    pnorm(y.test.avg, mean = mean(my.mu.y.given.x), sd = sqrt(var.y.avg))
  
  # Metropolis-Hastings ratio
  log.ratio <-
    log(MultivariateGaussian(x.proposal, mu.x, cov.mat.x, cov.mat.x.inv)) + log(new.y.avg.given.x) - log(MultivariateGaussian(previous.sample, mu.x, cov.mat.x, cov.mat.x.inv)) - log(old.y.avg.given.x)
  
  if (runif(1) < min(1, exp(log.ratio))) {
    return(x.proposal)
  }
  else {
    return(previous.sample)
  }
}

# Get samples from x given y average using MCMC
number.of.iterations <- 5000
samples <- vector('list', number.of.iterations)
sample.probabilities <- vector('list', number.of.iterations)
sample.acceptance <- vector('list', number.of.iterations)

x.current <-
  matrix(
    mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length ^ 2, ncol = 1, byrow = T
  )
for (i in 1:number.of.iterations) {
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

# Keep 50 samples
keep.samples <- seq(from = 1, to = number.of.iterations, by = 100)

sub.samples <- vector('list', length(keep.samples))

for (i in 1:length(keep.samples)) {
  sub.samples[[i]] <- samples[[keep.samples[i]]]
}

samples.mean <-
  (1 / length(sub.samples)) * sapply(1:grid.length ^ 2, function(x) {
    sum(sapply(sub.samples, function(sub.sample) {
      sub.sample[[x]]
    }))
  })

samples.mean.matrix <-
  matrix(samples.mean, nrow = grid.length, ncol = grid.length, byrow = T)

samples.mcmc <- sapply(1:length(sub.samples), function(index) {
  sub.samples[[index]]
})
# samples.matrix <- matrix(samples.mcmc, nrow = grid.length, ncol = grid.length, byrow = T)
# samples.var.matrix <- var(samples.matrix)
samples.var.matrix <- matrix(sapply(1:dim(samples.mcmc)[1], function(x) {
  var(samples.mcmc[x, ])
}), nrow = grid.length, ncol = grid.length, byrow = T)
filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               samples.var.matrix, color = kColours, 
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title('MCMC: Variance x given y average')

# filled.contour(1:grid.length, 1:grid.length, samples.mean.matrix, color = kColours)
filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               samples.mean.matrix, color = kColours, 
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title("MCMC: Mean of samples from x given y average")


# ABC approach
kTolerance <- 2
StatisticDistanceFunction <-
  function(proposed.sample.statistic, observed.statistic) {
    sqrt((proposed.sample.statistic - observed.statistic) ^ 2)
  }

number.of.abc.samples <- 50
abc.samples <- vector('list', number.of.abc.samples)
counter <- 1

while (counter <= length(abc.samples)) {
  abc.prior <- mvrnorm(mu = mu.x, Sigma = cov.mat.x)
  abc.prior.avg <- mean(abc.prior)
  if (StatisticDistanceFunction(abc.prior.avg, y.test.avg) < kTolerance) {
    print(paste("Got sample: ", counter))
    abc.samples[[counter]] <- abc.prior
    counter <- counter + 1
  }
}

abc.samples.mean <-
  sapply(1:grid.length ^ 2, function(x) {
    mean(sapply(abc.samples, function(abc.sample) {
      abc.sample[[x]]
    }))
  })

abc.samples.mean.matrix <- matrix(abc.samples.mean, nrow = grid.length, ncol = grid.length)
# filled.contour(1:grid.length, 1:grid.length, abc.samples.mean.matrix, color = kColours)
filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               abc.samples.mean.matrix, color = kColours, 
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title("Mean of samples from x given y average using ABC")


abc.samples.var.matrix <- matrix(sapply(1:grid.length^2, function(x) {
  var(sapply(abc.samples, function(abc.sample) {
    abc.sample[[x]]
  }))}), nrow = grid.length, ncol = grid.length, byrow = T)
filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               abc.samples.var.matrix, color = kColours, 
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title('ABC: Variance x given y average')


# Sampling from the prior
x.samples <- mvrnorm(n = 50, mu = mu.x, Sigma = cov.mat.x)
x.samples.mean <- matrix(sapply(1:grid.length^2, function(x) {
  mean(x.samples[ , x])
}), nrow = grid.length, ncol = grid.length, byrow = T)
# filled.contour(x.samples.mean, color = kColours)
filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               x.samples.mean, color = kColours)
title('Mean of 50 samples from x')

x.samples.var <- matrix(sapply(1:grid.length^2, function(x) {
  var(x.samples[ , x])
}), nrow = grid.length, ncol = grid.length, byrow = T)
# filled.contour(x.samples.var)
filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               x.samples.var, color = kColours)
title('Variance 50 samples from x')
