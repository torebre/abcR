
kObservation <- 0
kFinalEpsilon <- 0.01
kResampleRatio <- 0.5

# Uniform [-10, 10]
GenerateRandomSampleFromTheta <- function() {
  runif(1, min = -10, max = 10)
}

EvaluateLikelihood <- function(my.x, my.theta) {
  0.5 * dnorm(my.x, mean = my.theta, sd = 1) + 0.5 * dnorm(my.x, mean = my.theta, sd = 1 / 10)
}

GenrateSample <- function(my.theta) {
  0.5 * rnorm(1, mean = my.theta, sd = 1) + 0.5 * rnorm(1, mean = my.theta, sd = 1 / 10)
}


L1DistanceFromObservation <- function(my.sample) {
  abs(my.sample - kObservation)
}



ForwardKernelSample <- function(sample.old, theta.old) {
  temp <- sapply(sample.old, function(x) {x})
  dim(temp) <- NULL
  empirical.variance <- var(temp)
  
  new.theta <- rnorm(1, mean = theta.old, sqrt(2 * temp))
  
  new.samples <- rep(NA, kM)
  
  for(i in 1:kM) {
    new.samples[i] <- GenerateSample(new.theta)
  }
  
  
}




GenerateParticles <- function(my.number.of.particles, my.number.of.particles) {
  my.samples <- list()
  for(i in 1:my.number.of.particles) {
    theta <- GenerateRandomSampleFromTheta()
    my.replicates <- rep(NA, my.number.of.particles)
    for(j in 1:my.number.of.particles) {
      my.replicates[j] <- GenerateSample(theta)  
    }
    my.samples[[i]] <- my.replicates
  }
  return(my.samples)

}