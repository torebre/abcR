# ABC approach
kMaxTolerance <- 2
kTolerance <- 0.1

StatisticDistanceFunction <-
  function(proposed.sample.statistic, observed.statistic) {
    (proposed.sample.statistic - observed.statistic) ^ 2
  }

SamplePriorPhi <- function() {
  # 10
  runif(n = 1, min = 5, max = 15)
}

SamplePriorVariance <- function() {
  # 4
  runif(n = 1, min = 2, max = 6)
}

SamplePriorMean <- function() {
  # 10
  runif(n = 1, min = 6, max = 14)
}

SamplePriorNoise <- function() {
  # 2
  runif(n = 1, min = 1, max = 5)
}


number.of.abc.samples.y.avg <- 1000
abc.samples.y.avg <- list() #vector('list', number.of.abc.samples.y.avg)

counter <- 1
while (counter <= number.of.abc.samples.y.avg) {
  prior.phi <- SamplePriorPhi()
  prior.variance <- SamplePriorVariance()
  prior.mean <- SamplePriorMean()
  prior.obs.noise <- SamplePriorNoise()
  
  abc.mu.x <- matrix(rep(prior.mean, grid.length^2), ncol = 1)
  
  abc.cov.mat.x <- matrix(sapply(1:grid.length, function(x1) {
    sapply(1:grid.length, function(y1) {
      sapply(1:grid.length, function(x2) {
        sapply(1:grid.length, function(y2) {
          prior.variance * exp(-sqrt((x1 - x2)^2 + (y1 - y2)^2) / prior.phi)
        })
      })
    })
  }), nrow = grid.length^2, ncol = grid.length^2, byrow = T)
  
  # x given y average
  A <- matrix(1/dim(y.coords)[1], nrow = 1, ncol = dim(y.coords)[1])
  abc.mu.y.avg <- prior.mean
  abc.C <- solve(prior.obs.noise * A %*% t(A) + A %*% D %*% abc.cov.mat.x %*% t(D) %*% t(A))
  
  abc.mu.x.given.y.avg <- abc.mu.x + abc.cov.mat.x %*% t(D) %*% t(A) %*% abc.C %*% (matrix(y.avg - abc.mu.y.avg, nrow = 1, ncol = 1))
  abc.cov.mat.x.given.y.avg <- abc.cov.mat.x - abc.cov.mat.x %*% t(D) %*% t(A) %*% abc.C %*% A %*% D %*% abc.cov.mat.x
  
  abc.prior <- matrix(mvrnorm(mu =  abc.mu.x.given.y.avg , Sigma = abc.cov.mat.x), nrow = grid.length, ncol = grid.length)
  abc.prior.obs.points.mean <- mean(sapply(1:dim(y.coords)[1], function(obs.number) {
    abc.prior[y.coords[obs.number, 1], y.coords[obs.number, 2]]
  }))
  
  if(counter %% 1 == 0) {
    print(paste("Got sample: ", counter))  
  }
  
  abc.samples.y.avg[[counter]] <- list(abc.prior = abc.prior, prior.phi = prior.phi, prior.variance = prior.variance, prior.mean = prior.mean, prior.obs.noise = prior.obs.noise)
  counter <- counter + 1
}

abc.samples.y.avg.mean <-
  sapply(1:grid.length, function(x) {
    sapply(1:grid.length, function(y) {
      mean(sapply(abc.samples.y.avg, function(abc.sample) {
        abc.sample$abc.prior[x, y]
      }))})})

abc.samples.y.avg.mean.matrix <- matrix(abc.samples.y.avg.mean, nrow = grid.length, ncol = grid.length)

abc.samples.y.avg.var.matrix <- matrix(sapply(1:grid.length^2, function(x) {
  var(sapply(abc.samples.y.avg, function(abc.sample) {
    abc.sample[[x]]
  }))}), nrow = grid.length, ncol = grid.length, byrow = T)



GetMeanOfObservations <- function(abc.sample) {
  mean(sapply(1:dim(y.coords)[1], function(obs.point) {
    abc.sample[y.coords[obs.point, 1], y.coords[obs.point, 2]]
  }))
}



abc.distance.parameters.avg <- matrix(NA, nrow = length(abc.samples.y.avg), ncol = 5)
for(i in 1:length(abc.samples.y.avg)) {
  distance <- StatisticDistanceFunction(GetMeanOfObservations(abc.samples.y.avg[[i]]$abc.prior), y.avg)
  abc.distance.parameters.avg[i , ] = c(distance, abc.samples.y.avg[[i]]$prior.phi, abc.samples.y.avg[[i]]$prior.mean, abc.samples.y.avg[[i]]$prior.variance, abc.samples.y.avg[[i]]$prior.obs.noise)
}

# Phi
plot(abc.distance.parameters.avg[ , 2], abc.distance.parameters.avg[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\phi$'), ylab = latex2exp('$d(s(x),s(y))$'))

# Mean
plot(abc.distance.parameters.avg[ , 3], abc.distance.parameters.avg[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\mu$'), ylab = latex2exp('$d(s(x),s(y))$'))


# Variance
plot(abc.distance.parameters.avg[ , 4], abc.distance.parameters.avg[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\sigma^{2}$'), ylab = latex2exp('$d(s(x),s(y))$'))


# Observation noise
plot(abc.distance.parameters.avg[ , 5], abc.distance.parameters.avg[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\nu$'), ylab = latex2exp('$d(s(x),s(y))$'))
