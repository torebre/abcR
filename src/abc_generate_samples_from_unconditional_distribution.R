# ABC approach

# kTolerance <- 2
kMaxTolerance <- 2
kTolerance <- 0.1

StatisticDistanceFunctionAvg <-
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


number.of.abc.samples <- 1000
abc.samples <- list()
counter <- 1
all.samples.list <- list()

while (counter <= number.of.abc.samples) {
  prior.phi <- SamplePriorPhi()
  prior.variance <- SamplePriorVariance()
  prior.mean <- 10
  prior.obs.noise <- 0
  
  abc.cov.mat.x <- matrix(sapply(1:grid.length, function(x1) {
    sapply(1:grid.length, function(y1) {
      sapply(1:grid.length, function(x2) {
        sapply(1:grid.length, function(y2) {
          prior.variance * exp(-sqrt((x1 - x2)^2 + (y1 - y2)^2) / prior.phi)
        })
      })
    })
  }), nrow = grid.length^2, ncol = grid.length^2, byrow = T)
  
  abc.prior <- matrix(mvrnorm(mu = abc.mu.x, Sigma = abc.cov.mat.x), nrow = grid.length, ncol = grid.length, byrow = T)
  
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




# kVariableThreshold <- 0.1
# filtered.samples.mean.matrix <- matrix(0, nrow = grid.length, ncol = grid.length)
# number.of.filtered.samples <- 0
# for(i in 1:length(all.samples.list)) {
#   if(all.samples.list[[i]][[1]] <= kVariableThreshold) {
#     filtered.samples.mean.matrix <- filtered.samples.mean.matrix + all.samples.list[[i]][[2]]
#     number.of.filtered.samples <- number.of.filtered.samples + 1
#   }
# }
# filtered.samples.mean.matrix <- filtered.samples.mean.matrix / number.of.filtered.samples


# filled.contour(1:grid.length, 1:grid.length,
#                filtered.samples.mean.matrix, color = kColours, 
#                plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
# title(paste("ABC. Threshold: ", kVariableThreshold, "Samples: ", number.of.filtered.samples))


