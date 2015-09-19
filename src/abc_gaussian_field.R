library(MASS)

# Parameters
kPhi <- 5
kVariance <- 4
kMean <- 10
grid.length <- 10

kColours <- terrain.colors

# Functions for calculating the covariance
CovarianceFunction <- function(distance) {
  kVariance * exp(-distance / kPhi)
}

CalculateCovariance <-
  function(x1.coord, y1.coord, x2.coord, y2.coord)  {
    CovarianceFunction(sqrt((x1.coord - y1.coord)^2 + (x2.coord - y2.coord)^2))
  }

# Set up matrices for x and y
number.of.observations <- 3

x.number.of.points <- grid.length^2 - number.of.observations


# Choose some points where there are observations
y.coords = matrix(nrow = 3, ncol = 2)

y.coords[1, 1] <- 3
y.coords[1, 2] <- 5
y.coords[2, 1] <- 3
y.coords[2, 2] <- 7
y.coords[3, 1] <- 5
y.coords[3, 2] <- 7

observations <- matrix(c(20, 17, 20), nrow = 3, ncol = 1)

IsPointObservation <- function(i, j) {
  for(k in 1:number.of.observations) {
    if(y.coords[k, 1] == i && y.coords[k, 2] == j) {
      return(T)
    }
  }
  return(F)
}


x.coords <- matrix(NA, nrow = x.number.of.points, ncol = 2)
point.number <- 1
for(i in 1:grid.length) {
  for(j in 1:grid.length) {
    if(!IsPointObservation(i, j)) {
    x.coords[point.number, 1] <- i
    x.coords[point.number, 2] <- j
    point.number <- point.number + 1
    }
  }
}





samples.x.given.y <- mvrnorm(n = 50, mu = mu.x.given.y, Sigma = cov.mat.x.given.y)
mean.samples.x.given.y <- sapply(1:x.number.of.points, function(x) {
  mean(samples.x.given.y[ , x])
})

# TODO The dimensions are wrong in the following
# mean.samples.x.given.y.matrix <- matrix(c(mean.samples.x.given.y, rep(0, 3)), nrow = 10, byrow = T)
# filled.contour(1:grid.length, 1:grid.length, mean.samples.x.given.y.matrix, plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
# 
# var.samples.x.given.y.matrix <- matrix(sapply(1:x.number.of.points, function(x) {
#   var(samples.x.given.y[ , x])
# }), nrow = x.number.of.points, ncol = x.number.of.points, byrow = T)


result <- matrix(NA, nrow = grid.length, ncol = grid.length, byrow = T)
# CombinePredictionAndObservationMatrix <- function(my.x.values, my.y.values) {
  x.counter <- 1
  y.counter <- 1
  for(i in 1:grid.length) {
    for(j in 1:grid.length) {
      if(IsPointObservation(i, j)) {
        result[i, j] <- observations[y.counter]
        y.counter <- y.counter + 1
      }
      else {
        # print(my.x.values[x.counter])
        result[i, j] <- mean.samples.x.given.y[x.counter]
#         print(paste('i', i, 'j', j))
#         print(result[i, j])
        x.counter <- x.counter + 1
      }
    }
  }
# }


# combined.matrix <- CombinePredictionAndObservationMatrix(mean.samples.x.given.y, observations)
filled.contour(1:grid.length, 1:grid.length, result, color = kColours,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))




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



filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               matrix(mu.x.given.y, nrow = 10, ncol = 10), color = kColours, 
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))


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
burn.in.period <- 2000
number.of.iterations <- 50000
samples <- vector('list', number.of.iterations)
sample.probabilities <- vector('list', number.of.iterations)
sample.acceptance <- vector('list', number.of.iterations)

x.current <-
  matrix(
    mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length ^ 2, ncol = 1, byrow = T
  )
for (i in 1:number.of.iterations) {
  x.new <- RandomWalkMetropolisHastingsMCMC(x.current)
  
  if(i %% 5000 == 0) {
    print(i)
  }
  
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
keep.samples <- seq(from = burn.in.period, to = number.of.iterations, by = 1000)

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

plot(1:length(sample.probabilities), sample.probabilities, type = 'l')
title('Sample probabilities')

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

png('mean_comparisons.png')
par(mfrow = c(2, 2))
filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               mean.samples.x.given.y.matrix, color = kColours, 
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title('Mean of samples from x given y')

filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               samples.mean.matrix, color = kColours, 
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title("MCMC: Mean of samples from x given y average")

filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               abc.samples.mean.matrix, color = kColours, 
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title("Mean of samples from x given y average using ABC")

filled.contour(seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               seq(1, kDistance.between.gridlines * grid.length, kDistance.between.gridlines), 
               x.samples.mean, color = kColours,
               plot.axes = points(obs.coords[ , 1], obs.coords[ , 2], pch = 19))
title('Mean of 50 samples from x')

dev.off()
