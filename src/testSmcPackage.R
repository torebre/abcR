

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
  if(runif(1) < 0.5) {
    rnorm(1, mean = my.theta, sd = 1)
  }
  else {
    rnorm(1, mean = my.theta, sd = 1 / 10)
  }
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
kNumberOfParticles <- 10000

debug.variables <- new.env(parent = emptyenv())
debug.variables$avg.acc.rate <- list()
debug.variables$empirical.variance <- list()
debug.variables$alive.particles <- list()
debug.variables$accepted <- list()


ForwardKernelSample <-
  function(samples.old,
           theta.old,
           my.current.epsilon,
           my.weights) {
    # TODO Will this work if there are multiple replicates of a theta?
    temp <- sapply(theta.old, function(x) {
      x
    })
    dim(temp) <- NULL
    empirical.variance <- var(temp) # temp[my.weights > 0])

    # mean.theta <- mean(theta.old)
    # empirical.variance <- 0
    # for(i in 1:kNumberOfParticles) {
    #   empirical.variance <- empirical.variance + my.weights[i] * (theta.old[i] - mean.theta)^2
    # }

    samples.new <- rep(samples.old)
    theta.new <- rep(theta.old)

    accepted <- 0
    alive.particles <- 0

    for (j in 1:kNumberOfParticles) {
      # print(paste("my.weights[", j, "] = ", my.weights[j]))

      # if (my.weights[j] <= 0) {
      #   next
      # }

      # for (k in 1:100) {
      theta.candidate <-
        rnorm(1, mean = theta.old[j], sqrt(2 * empirical.variance))

      debug.variables$empirical.variance[[length(debug.variables$avg.acc.rate) + 1]] <- sqrt(2 * empirical.variance)

      replicates.new <- rep(NA, kNumberOfReplicates)

      for (i in 1:kNumberOfReplicates) {
        replicates.new[i] <- GenerateSample(theta.candidate)
      }

      #   prior.old <- EvaluateTheta(theta.old)
      #   prior.new <- EvaluateTheta(theta.new)

      # The prior is uniform and the random walk is coming from
      # a symmetric distribution so the only term left in the
      # Metropolis-Hastings ratio is the likelihood

      if (my.weights[j] == 0) {
        theta.new[j] <- theta.candidate
        samples.new[[j]] <- replicates.new
        # accepted <- accepted + 1
      }
      else {
        # New as nominator, old as denominator

        old.likelihood <- EvaluateLikelihoodSum(samples.new[j], my.current.epsilon)

        if(old.likelihood == 0) {
          next
        }

        metropolis.hastings.ratio <-
          EvaluateLikelihoodSum(replicates.new, my.current.epsilon) / old.likelihood

        #       print(paste("New: ", EvaluateLikelihoodSum(replicates.new, my.current.epsilon)))
        #       print(paste("Old: " ,EvaluateLikelihoodSum(samples.old[j], my.current.epsilon)))

        if (runif(1) <= min(1, metropolis.hastings.ratio)) {
          theta.new[j] <- theta.candidate
          samples.new[[j]] <- replicates.new

          accepted <- accepted + 1
        }

        alive.particles <- alive.particles + 1

      }

      # }

    }

    debug.variables$accepted[[length(debug.variables$accepted) + 1]] <- accepted
    debug.variables$avg.acc.rate[[length(debug.variables$avg.acc.rate) + 1]] <- accepted / alive.particles
    debug.variables$alive.particles[[length(debug.variables$alive.particles) + 1]] <- alive.particles

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

results <-
  Smc(
    max.iterations = 100000,
    alpha = 0.95,
    number.of.particles = kNumberOfParticles,
    resample.ratio = 0.5,
    start.epsilon = 10,
    stop.epsilon = 0.01,
    number.of.replicates = 1,
    SampleFunction,
    ForwardKernelSample,
    DistanceFunction,
    GenerateRandomSampleFromTheta
  )

iteration.length <- length(results$all.thetas)
result.thetas <- unlist(results$all.thetas[iteration.length])
hist(
  result.thetas,
  breaks = seq(from = -3, to = 3, by = 0.1),
  freq = F,
  ylim = c(0, 2.5),
  ann = F
)

# Prior is uniform, and is not included here
Posterior <- function(my.theta) {
  0.5 * dnorm(kObservation, mean = my.theta, sd = 1) + 0.5 * dnorm(kObservation, mean = my.theta, sd = 1 / 10)
}
VectorizedPosterior <- Vectorize(Posterior)
curve(VectorizedPosterior,
      from = -3,
      to = 3,
      add = T)


# plot(unlist(debug.variables$avg.acc.rate), type = "l")
