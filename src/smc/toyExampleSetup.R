

kObservation <- 0
kFinalEpsilon <- 0.01
kResampleRatio <- 0.5

# Uniform [-10, 10]
GenerateRandomSampleFromTheta <- function() {
  runif(1, min = -10, max = 10)
}

EvaluateTheta <- function() {
  # The distribution is uniform
  1 / 20
}

EvaluateLikelihood <- function(my.x, my.theta) {
  0.5 * dnorm(my.x, mean = my.theta, sd = 1) + 0.5 * dnorm(my.x, mean = my.theta, sd = 1 / 10)
}

GenerateSample <- function(my.theta) {
  0.5 * rnorm(1, mean = my.theta, sd = 1) + 0.5 * rnorm(1, mean = my.theta, sd = 1 / 10)
}

L1DistanceFromObservation <- function(my.sample) {
  abs(my.sample - kObservation)
}

EvaluateLikelihoodSum <- function(my.sample.vector, my.current.epsilon) {
  sum(sapply(my.sample.vector, function(sample) {L1DistanceFromObservation(sample) < my.current.epsilon}))
}


ForwardKernelSample <- function(sample.old, theta.old, my.current.epsilon) {
  temp <- sapply(sample.old, function(x) {x})
  dim(temp) <- NULL
  empirical.variance <- var(temp)
  
  theta.new <- rnorm(1, mean = theta.old, sqrt(2 * temp))
  samples.new <- rep(NA, kNumberOfReplicates)
  
  for(i in 1:kNumberOfReplicates) {
    samples.new[i] <- GenerateSample(theta.new)
  }
  
#   prior.old <- EvaluateTheta(theta.old)
#   prior.new <- EvaluateTheta(theta.new)
  
  # The prior is uniform and the random walk is coming from 
  # a symmetric distribution so the only term left in the 
  # Metropolis-Hastings ratio is the likelihood
  
  # New as nominator, old as denominator
  metropolis.hastings.ratio <- EvaluateLikelihoodSum(samples.new, samples.old)
  
  if(runif(1) <= min(1, metropolis.hastings.ratio)) {
    return(list(theta = theta.new, samples = samples.new))
  }
  
  return(list(theta = theta.old, samples = samples.old))
}


GenerateParticles <- function(my.thetas, my.number.of.replicates) {
  my.samples <- list()
  for(i in 1:length(my.thetas)) {
    my.replicates <- rep(NA, my.number.of.replicates)
    for(j in 1:my.number.of.replicates) {
      my.replicates[j] <- GenerateSample(my.thetas[i])  
    }
    my.samples[[i]] <- my.replicates
  }
  return(my.samples)
}
