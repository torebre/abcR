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
  
  
  abc.prior <- matrix(mvrnorm(mu = mu.x, Sigma = cov.mat.x), nrow = grid.length, ncol = grid.length)
  abc.prior.obs.points.mean <- mean(sapply(1:dim(y.coords)[1], function(obs.number) {
    abc.prior[y.coords[obs.number, 1], y.coords[obs.number, 2]]
  }))
  
  # distance <- StatisticDisatanceFunction(abc.prior.obs.points.mean, y.avg)
  # if (distance <= kTolerance) {
    # print(paste("Got sample: ", counter))
  
  if(counter %% 50 == 0) {
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
# filled.contour(1:grid.length, 1:grid.length, abc.samples.mean.matrix, color = kColours)
# filled.contour(1:grid.length, 1:grid.length,
#                abc.samples.mean.matrix, color = kColours, 
#                plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
# text = parse(text = paste("hat(x,bar(y))"))
# title(latex2exp('ABC samples $\\hat{E(x|\\bar{y})}$'))

# title("hat(E)")


abc.samples.var.matrix <- matrix(sapply(1:grid.length^2, function(x) {
  var(sapply(abc.samples, function(abc.sample) {
    abc.sample[[x]]
  }))}), nrow = grid.length, ncol = grid.length, byrow = T)
# filled.contour(1:grid.length, 
#                1:grid.length, 
#                abc.samples.var.matrix, color = kColours, 
#                plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
# title('ABC: Variance x given y average')



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


