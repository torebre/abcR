kObservation <- 0
kFinalEpsilon <- 0.01
kResampleRatio <- 0.5
kStopEpsilon <- 0.01


GenerateRandomSampleFromTheta <- function() {
  # Phi = 10 and variance = 4 when creating the observation
  c(runif(n = 1, min = 8, max = 12), runif(n = 1, min = 2, max = 6))
}

GenerateSample <- function(my.theta) {
  prior.phi <- my.theta[1]
  prior.variance <- my.theta[2]
  prior.mean <- 10
  prior.obs.noise <- 0
  
  print(paste(prior.phi, prior.variance))
  temp <- grf(grid.length^2, all.sample.points, cov.model = "exp", cov.pars = c(prior.variance, prior.phi), mean = kMean)
  matrix(temp$data, nrow = grid.length, ncol = grid.length, byrow = T)
}

DistanceFunction <- function(my.sample) {
  sum((actual.data.empirical.variogram.all.points$v - my.sample$variogram$v)^2)
}

EvaluateLikelihoodSum <-
  function(my.sample.vector, my.current.epsilon) {
    # Assuming no replicates for debugging
    DistanceFunction(my.sample.vector) < my.current.epsilon
#     sum(sapply(my.sample.vector, function(x) {
#       #       print(paste("x: ", x))
#       #       print(paste("Distance: ", DistanceFunction(x)))
#       #       print(paste("my.current.epsilon: ", my.current.epsilon))
#       
#       DistanceFunction(x) < my.current.epsilon
#     }))
  }


ForwardKernelSample <-
  function(samples.old, theta.old, my.current.epsilon, my.weights) {
    temp <- sapply(samples.old, function(x) {
      x
    })
    dim(temp) <- NULL
    
    my.phi <- rep(NA, length(samples.old))
    my.variance <- rep(NA, length(samples.old))
    
    # Commented out for debugging
    # sink("/dev/null")
#     for(i in 1:length(samples.old)) {
#       # print(paste("Looking at sample ", i))
#       # transformed.sample <- TransformSampleToFormatForVariogramFunction(samples.old[[i]])
#       # my.empirical.variogram <- variog(coord = transformed.sample$sample.coords, data = transformed.sample$sample.all.observations, breaks = bins.limits)
#       # my.variograms[[i]] <- my.empirical.variogram
#       
#       my.model.fit <- variofit(samples.old[[i]]$variogram, cov.model = "exponential")
#       my.variance[i] <- my.model.fit$cov.pars[1]
#       my.phi[i] <- my.model.fit$cov.pars[2]
#     }
    # sink()
    
    # Setting some constant values for debugging
    empirical.variance <- c(1, 1) #c(var(my.phi), var(my.variance))
    
    samples.new <- rep(samples.old)
    theta.new <- matrix(rep(theta.old), nrow = dim(theta.old)[1], ncol = dim(theta.old)[2])
    
    for (j in 1:kNumberOfParticles) {
      if (my.weights[j] <= 0) {
        next
      }
      
      # for (k in 1:100) {
      theta.candidate <- c(abs(rnorm(1, mean = theta.old[j, 1], sqrt(2 * empirical.variance[1]))), abs(rnorm(1, mean = theta.old[j, 2], sqrt(2 * empirical.variance[2]))))
      
      # replicates.new <- rep(NA, kNumberOfReplicates)
      
      # For debugging assuming no replicates here
      # for (i in 1:kNumberOfReplicates) {
        
        # sink("/dev/null")
        replicates.new <- GenerateSample(theta.candidate)
        # sink()
        
      # }
      
      
      #   prior.old <- EvaluateTheta(theta.old)
      #   prior.new <- EvaluateTheta(theta.new)
      
      # The prior is uniform and the random walk is coming from
      # a symmetric distribution so the only term left in the
      # Metropolis-Hastings ratio is the likelihood
      
      # New as nominator, old as denominator
      transformed.sample <- TransformSampleToFormatForVariogramFunction(replicates.new)
      transformed.sample.variogram <- variog(coord = transformed.sample$sample.coords, data = transformed.sample$sample.all.observations, breaks = bins.limits)
      metropolis.hastings.ratio <-
        EvaluateLikelihoodSum(list(sample = transformed.sample, variogram = transformed.sample.variogram), my.current.epsilon) / EvaluateLikelihoodSum(samples.new[[j]], my.current.epsilon)
      
      #       print(paste("New: ", EvaluateLikelihoodSum(replicates.new, my.current.epsilon)))
      #       print(paste("Old: " ,EvaluateLikelihoodSum(samples.old[j], my.current.epsilon)))
      
      if (runif(1) <= min(1, metropolis.hastings.ratio)) {
        theta.new[j, ] <- theta.candidate
        samples.new[[j]] <- list(sample = replicates.new, variogram = transformed.sample.variogram)
      }
      
      # }
      
    }
    
    return(list(theta = theta.new, samples = samples.new))
  }



GenerateParticles <- function(my.thetas, my.number.of.replicates) {
  my.samples <- list()
  
  # sink("/dev/null")
  for (i in 1:dim(my.thetas)[1]) {
    # my.replicates <- rep(NA, my.number.of.replicates)
#     for (j in 1:my.number.of.replicates) {
#       my.replicates[j] <- GenerateSample(my.thetas[i, ])
#     }
    
    my.sample <- GenerateSample(my.thetas[i, ])
    my.transformed.sample <- TransformSampleToFormatForVariogramFunction(my.sample)
    my.empirical.variogram <- variog(coord = my.transformed.sample$sample.coords, data = my.transformed.sample$sample.all.observations, breaks = bins.limits)
    
    my.samples[[i]] <- list(sample = my.sample, variogram = my.empirical.variogram)
  }
  # sink()
  return(my.samples)
}


TransformSampleToFormatForVariogramFunction2 <- function(my.sample) {
  my.sample.dims <- dim(my.sample)
  my.sample.coords <- matrix(NA, my.sample.dims[1] * my.sample.dims[2], 2)
  my.sample.all.observations <- rep(NA, my.sample.dims[1] * my.sample.dims[2])
  
  counter <- 1
  
  for(i in 1:my.sample.dims[1]) {
    for(j in 1:my.sample.dims[2]) {
      # current.row <- (i - 1) * my.sample.dims[1] + j
      current.row <- counter
      counter <- counter + 1
      my.sample.coords[current.row, 1] <- i
      my.sample.coords[current.row, 2] <- j
      my.sample.all.observations[current.row] <- my.sample[i, j]
    }
  }
  list(sample.coords = my.sample.coords, sample.all.observations = my.sample.all.observations)
}

