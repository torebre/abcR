# ABC approach

# Contains code for generating ABC samples

SamplePriorPhi <- function() {
  # 10
  runif(n = 1, min = 5, max = 25)
}

SamplePriorVariance <- function() {
  # 4
  runif(n = 1, min = 2, max = 20)
}


number.of.abc.samples <- 1000
abc.samples <- list()
counter <- 1
all.samples.list <- list()


all.sample.points <- matrix(NA, nrow = grid.length^2, ncol = 2)
for(i in 1:grid.length) {
  for(j in 1:grid.length) {
    all.sample.points[(i - 1) * grid.length + j, ] <- c(i, j)
  }
}


while (counter <= number.of.abc.samples) {
  prior.phi <- SamplePriorPhi()
  prior.variance <- SamplePriorVariance()
  prior.mean <- 10
  prior.obs.noise <- 0
  
  temp <- grf(grid.length^2, all.sample.points, cov.model = "exp", cov.pars = c(prior.variance, prior.phi), mean = kMean)
  abc.prior <- matrix(temp$data, nrow = grid.length, ncol = grid.length, byrow = T)
  
  # abc.prior <- matrix(mvrnorm(mu = abc.mu.x, Sigma = abc.cov.mat.x), nrow = grid.length, ncol = grid.length, byrow = T)
  
  if(counter %% 1 == 0) {
    print(paste("Got sample: ", counter))  
  }
  
  abc.samples[[counter]] <- list(abc.prior = abc.prior, prior.phi = prior.phi, prior.variance = prior.variance, prior.mean = prior.mean, prior.obs.noise = prior.obs.noise)
  counter <- counter + 1
}

abc.samples.mean <-
  sapply(1:grid.length, function(x) {
    sapply(1:grid.length, function(y) {
      mean(sapply(abc.samples, function(abc.sample) {
        abc.sample$abc.prior[x, y]
      }))})})

abc.samples.mean.matrix <- matrix(abc.samples.mean, nrow = grid.length, ncol = grid.length, byrow = T)

# abc.samples.var <- sapply(1:grid.length, function(x) {
#     sapply(1:grid.length, function(y) {
#       var(sapply(abc.samples, function(abc.sample) {
#         abc.sample$prior[x, y]
#     }))})})

abc.samples.var <-
  sapply(1:grid.length, function(x) {
    sapply(1:grid.length, function(y) {
      var(sapply(abc.samples, function(abc.sample) {
        abc.sample$abc.prior[x, y]
      }))})})


abc.samples.var.matrix <- matrix(abc.samples.var, nrow = grid.length, ncol = grid.length)

GetMeanOfObservations <- function(abc.sample) {
  mean(sapply(1:dim(y.coords)[1], function(obs.point) {
    abc.sample[y.coords[obs.point, 1], y.coords[obs.point, 2]]
  }))
}

abc.distance.parameters.avg <- matrix(NA, nrow = length(abc.samples), ncol = 5)
for(i in 1:length(abc.samples)) {
  distance <- StatisticDistanceFunction(GetMeanOfObservations(abc.samples[[i]]$abc.prior), y.avg)
  abc.distance.parameters.avg[i , ] = c(distance, abc.samples[[i]]$prior.phi, abc.samples[[i]]$prior.mean, abc.samples[[i]]$prior.variance, abc.samples[[i]]$prior.obs.noise)
}

# Phi
plot(abc.distance.parameters[ , 2], abc.distance.parameters[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\phi$'), ylab = latex2exp('$d(s(x),s(y))$'))

# Mean
plot(abc.distance.parameters[ , 3], abc.distance.parameters[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\mu$'), ylab = latex2exp('$d(s(x),s(y))$'))

# Variance
plot(abc.distance.parameters[ , 4], abc.distance.parameters[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\sigma^{2}$'), ylab = latex2exp('$d(s(x),s(y))$'))


# Observation noise
plot(abc.distance.parameters[ , 5], abc.distance.parameters[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\nu$'), ylab = latex2exp('$d(s(x),s(y))$'))
