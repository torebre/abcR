# ABC approach

# kTolerance <- 2
kMaxTolerance <- 2
kTolerance <- 0.1
StatisticDistanceFunction <-
  function(proposed.sample.statistic, observed.statistic) {
    sqrt((proposed.sample.statistic - observed.statistic) ^ 2)
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



number.of.abc.samples <- 10000
abc.samples <- list() #vector('list', number.of.abc.samples)
counter <- 1
all.samples.list <- list()


parameter.values <- matrix(sapply(seq(1, 5, 15), function(prior.phi) {
  sapply(seq(2, 6, 1), function(prior.variance) {
    sapply(seq(6, 14, 1), function(prior.mean) {
      sapply(seq(1, 5, 1), function(prior.noise) {
        list(prior.phi = prior.phi, prior.variance = prior.variance, prior.mean = prior.mean, prior.noise = prior.noise)    
      })
    })
  })
}), nrow = 4)



while (counter <= number.of.abc.samples) {
  prior.phi <- SamplePriorPhi()
  prior.variance <- SamplePriorVariance()
  prior.mean <- SamplePriorMean()
  prior.obs.noise <- SamplePriorNoise()
  
  abc.mu.y <- matrix(rep(prior.mean, dim(y.coords)[1]), ncol = 1)
  abc.cov.mat.y.given.x <- prior.obs.noise * diag(dim(y.coords)[1])
  
  abc.mu.x <- matrix(rep(prior.mean, grid.length^2), ncol = 1)
  
  abc.cov.mat.x <- matrix(sapply(1:grid.length, function(x1) {
    sapply(1:grid.length, function(y1) {
      sapply(1:grid.length, function(x2) {
        sapply(1:grid.length, function(y2) {
          CalculateCovariance(x1, x2, y1, y2)    
        })
      })
    })
  }), nrow = grid.length^2, ncol = grid.length^2, byrow = T)
  
  
  abc.C2 <- solve(abc.cov.mat.y.given.x + D %*% abc.cov.mat.x %*% t(D))
  abc.mu.x.given.y <- abc.mu.x + abc.cov.mat.x %*% t(D) %*% abc.C2 %*% matrix(observations - abc.mu.y, nrow = length(abc.mu.y), ncol = 1)  
  abc.cov.mat.x.given.y <- abc.cov.mat.x - abc.cov.mat.x %*% t(D) %*% abc.C2 %*% D %*% abc.cov.mat.x
  
  
  abc.prior <- matrix(mvrnorm(mu = abc.mu.x, Sigma = abc.cov.mat.x), nrow = grid.length, ncol = grid.length)
  abc.prior.obs.points.mean <- mean(sapply(1:dim(y.coords)[1], function(obs.number) {
    abc.prior[y.coords[obs.number, 1], y.coords[obs.number, 2]]
  }))
  
  # distance <- StatisticDistanceFunction(abc.prior.obs.points.mean, y.avg)
  # if (distance <= kTolerance) {
    # print(paste("Got sample: ", counter))
  
  if(counter %% 1 == 0) {
    print(paste("Got sample: ", counter))  
  }
  
    abc.samples[[counter]] <- list(abc.prior = abc.prior, prior.phi = prior.phi, prior.variance = prior.variance, prior.mean = prior.mean, prior.obs.noise = prior.obs.noise)
    counter <- counter + 1
  # }
  
  
#   if (distance <= kMaxTolerance) {
#     all.samples.list <- append(all.samples.list, list(list(distance, abc.prior)))
#   }
  
  
}

abc.samples.mean <-
  sapply(1:grid.length, function(x) {
    sapply(1:grid.length, function(y) {
    mean(sapply(abc.samples, function(abc.sample) {
      abc.sample$abc.prior[x, y]
    }))})})

abc.samples.mean.matrix <- matrix(abc.samples.mean, nrow = grid.length, ncol = grid.length)

abc.samples.var.matrix <- matrix(sapply(1:grid.length^2, function(x) {
  var(sapply(abc.samples, function(abc.sample) {
    abc.sample[[x]]
  }))}), nrow = grid.length, ncol = grid.length, byrow = T)



GetMeanOfObservations <- function(abc.sample) {
  mean(sapply(1:dim(y.coords)[1], function(obs.point) {
    abc.sample[y.coords[obs.point, 1], y.coords[obs.point, 2]]
  }))
}


abc.distance.parameters <- matrix(NA, nrow = length(abc.samples), ncol = 5)
for(i in 1:length(abc.samples)) {
  distance <- StatisticDistanceFunction(GetMeanOfObservations(abc.samples[[i]]$abc.prior), y.avg)
  abc.distance.parameters[i , ] = c(distance, abc.samples[[i]]$prior.phi, abc.samples[[i]]$prior.mean, abc.samples[[i]]$prior.variance, abc.samples[[i]]$prior.obs.noise)
}






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


