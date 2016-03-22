
kObservation <- 0
kNumberOfReplicates <- 1


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

DistanceFunction <- function(my.sample) {
  # print(paste("Sample: ", my.sample))
  abs(my.sample - kObservation)
}

EvaluateLikelihoodSum <-
  function(my.sample.vector, my.current.epsilon) {
    sum(sapply(my.sample.vector, function(x) {
      #       print(paste("x: ", x))
      #       print(paste("Distance: ", DistanceFunction(x)))
      #       print(paste("my.current.epsilon: ", my.current.epsilon))

      DistanceFunction(x) < my.current.epsilon
    }))
  }

# TODO Do not set number of particles here
kNumberOfParticles <- 100

ForwardKernelSample <-
  function(samples.old, theta.old, my.current.epsilon, my.weights) {
    temp <- sapply(samples.old, function(x) {
      x
    })
    dim(temp) <- NULL
    empirical.variance <- var(temp)

    samples.new <- rep(samples.old)
    theta.new <- rep(theta.old)

    for (j in 1:kNumberOfParticles) {
      if (my.weights[j] <= 0) {
        next
      }

      # for (k in 1:100) {
      theta.candidate <-
        rnorm(1, mean = theta.old[j], sqrt(2 * empirical.variance))

      replicates.new <- rep(NA, kNumberOfReplicates)

      for (i in 1:kNumberOfReplicates) {
        replicates.new[i] <- GenerateSample(theta.candidate)
      }


      #   prior.old <- EvaluateTheta(theta.old)
      #   prior.new <- EvaluateTheta(theta.new)

      # The prior is uniform and the random walk is coming from
      # a symmetric distribution so the only term left in the
      # Metropolis-Hastings ratio is the likelihood

      # New as nominator, old as denominator
      metropolis.hastings.ratio <-
        EvaluateLikelihoodSum(replicates.new, my.current.epsilon) / EvaluateLikelihoodSum(samples.new[j], my.current.epsilon)

      #       print(paste("New: ", EvaluateLikelihoodSum(replicates.new, my.current.epsilon)))
      #       print(paste("Old: " ,EvaluateLikelihoodSum(samples.old[j], my.current.epsilon)))

      if (runif(1) <= min(1, metropolis.hastings.ratio)) {
        theta.new[j] <- theta.candidate
        samples.new[[j]] <- replicates.new
      }

      # }

    }

    return(list(theta = theta.new, samples = samples.new))
  }


SampleFunction <- function(my.thetas, my.number.of.replicates) {
  my.samples <- list()
  for (i in 1:length(my.thetas)) {
    my.replicates <- rep(NA, my.number.of.replicates)
    for (j in 1:my.number.of.replicates) {
      my.replicates[j] <- GenerateSample(my.thetas[i])
    }
    my.samples[[i]] <- my.replicates
  }
  return(my.samples)
}

Smc(max.iterations = 1000, alpha = 0.9, number.of.particles = 100, resample.ratio = 0.5, stop.epsilon = 0.001, initial.particles = NULL,
                number.of.replicates = 1,
                SampleFunction, ForwardKernelSample, DistanceFunction, GenerateRandomSampleFromTheta)
