


# SMC
#' @export
Smc <- function(max.iterations = 40, alpha = 0.9, number.of.particles = 100, resample.ratio = 0.5, stop.epsilon = 0.1, initial.particles = NULL,
                number.of.replicates = 1,
                SampleFunction, ForwardKernelSample, DistanceFunction, GenerateRandomSampleFromTheta) {
  # Create an initial set of particles
  thetas <- matrix(sapply(1:number.of.particles, function(x) { GenerateRandomSampleFromTheta() }), nrow = number.of.particles, ncol = 1)
  particles <- SampleFunction(thetas, number.of.replicates)

  counter <- 0
  resample.limit <- number.of.particles * resample.ratio


  # previous.inclusion.sum <- CalculateInclusionSum(samples, DistanceFunction, current.epsilon, kObservation)
  weights <- rep(1 / number.of.particles, number.of.particles)
  # previous.effective.sample.size <- ComputeEffectiveSampleSize(previous.weights)
  effective.sample.size <- number.of.particles
  current.epsilon <- 1000000

  # temp.function <- Vectorize(function(x) {FindNextEpsilon(x, current.epsilon, particles, weights)})
  # curve(temp.function, from = 1, to = 15)

  effective.sample.sizes <- list()
  epsilons <- list()
  all.thetas <- list()
  all.weights <- list()
  all.particles <- list()

  counter <- 1

  while (T) {
    # Adaptation
    print(paste("Current epsilon: ", current.epsilon))
    epsilons[[counter]] <- current.epsilon
    effective.sample.sizes[[counter]] <- effective.sample.size
    all.thetas[[counter]] <- thetas
    all.weights[[counter]] <- weights
    all.particles[[counter]] <- unlist(particles)

    if(current.epsilon <= stop.epsilon) {
      break
    }

    # Find next epsilon
    epsilon.new <- uniroot(function(epsilon.candidate) {
      FindNextEpsilon(epsilon.candidate, current.epsilon, particles, weights, alpha)
    }, extendInt = "no", c(0, current.epsilon))$root

    # TODO Only compute the weights once, not here and in FindNextEpsilon
    weights <- NormalizeVector(weights * CalculateWeightUpdates(particles, current.epsilon, epsilon.new, DistanceFunction))
    if(epsilon.new < current.epsilon) {
      current.epsilon <- epsilon.new
    }

    effective.sample.size <- ComputeEffectiveSampleSize(weights)

    # Resampling
    if(effective.sample.size < resample.ratio * number.of.particles) {

      print("Resampling")
      resampling.indices <- sample(1:number.of.particles, number.of.particles, replace = T, prob = weights)
      particles <- particles[resampling.indices]
    # TODO Figure why thetas was a matrix with two columns
      thetas <- thetas[resampling.indices]
      weights <- rep(1 / number.of.particles, number.of.particles)
    }

    # Mutation
    # sink("/dev/null")
    iterated.samples <- ForwardKernelSample(particles, thetas, current.epsilon, weights)
    # sink()

    particles <- iterated.samples$samples
    thetas <- iterated.samples$theta

    counter <- counter + 1

    print(paste("Counter: ", counter, " Effective sample size: ", effective.sample.size))

    if(counter == max.iterations) {
      break
    }
  }

}

ComputeEffectiveSampleSize <- function(weights) {
  1 / sum(weights^2)
}


FindNextEpsilon <- function(epsilon.candidate, my.current.epsilon, my.particles, my.previous.weights, alpha) {
  ess.old <- ComputeEffectiveSampleSize(my.previous.weights)

  # print(paste("Epsilon candidate: ", epsilon.candidate))

  weight.updates <- CalculateWeightUpdates(my.particles, my.current.epsilon, epsilon.candidate, DistanceFunction)

  if(sum(weight.updates) == 0) {
    return(-1)
  }

  # print(paste("Weight updates: ", weight.updates))

  weights.new <- NormalizeVector(my.previous.weights * weight.updates)

  #   print(paste("Previous weights: ", my.previous.weights))
  #   print(paste("Weights new: ", weights.new))

  ess.new <- ComputeEffectiveSampleSize(weights.new)

  #   print(paste(ess.new, ", ", alpha, ", ", ess.old))
  #   print(paste("New epsilon: ", ess.new - alpha * ess.old))

  return(ess.new - alpha * ess.old)
}

NormalizeVector <- function(my.vector) {
  my.vector.sum <- sum(my.vector)
  my.vector / my.vector.sum

  #   my.vector.min <- min(my.vector)
  #   my.vector.range <- max(my.vector) - my.vector.min
  #   # TODO Have some other cutoff criteria?
  #   if(my.vector.range == 0) {
  #     return(rep(1 / length(my.vector), length(my.vector)))
  #   }
  #   sapply(my.vector, function(x) {
  #     (x - my.vector.min) / my.vector.range
  #   })
}


CalculateWeightUpdates <- function(my.samples, my.old.epsilon, my.new.epsilon, DistanceFunction) {
  sapply(1:length(my.samples), function(x) { CalculateWeightUpdateForParticle(my.samples, x, my.old.epsilon, my.new.epsilon, DistanceFunction)})
}

CalculateWeightUpdateForParticle <- function(my.samples, my.particle.number, my.old.epsilon, my.new.epsilon, DistanceFunction) {
  sum1 <- CalculateInclusionSum(my.samples[my.particle.number], DistanceFunction, my.new.epsilon)
  sum2 <- CalculateInclusionSum(my.samples[my.particle.number], DistanceFunction, my.old.epsilon)


  #   print(paste("sum1: ", sum1, "sum2: ", sum2))
  #   print(paste("my.old.epsilon: ", my.old.epsilon))
  #   print(paste("my.new.epsilon: ", my.new.epsilon))


  if(sum1 == 0) {
    return(0)
  }

  if(sum1 == sum2) {
    return(1)
  }

  return(sum1 / sum2)
}


CalculateInclusionSum <- function(my.sample.replicates, DistanceFunction, my.epsilon) {
  sum(sapply(my.sample.replicates, function(x) {

    #       print(paste("x: ", x))
    #       print(paste("Distance: ", DistanceFunction(x)))
    #       print(paste("my.epsilon: ", my.epsilon))

    DistanceFunction(x) < my.epsilon
  }))
  #
  #     weights <- rep(NA, length(my.sample.replicates))
  #     for (i in 1:length(my.samples)) {
  #       # temp <- c(DistanceFunction(my.samples[[i]][1, , ], my.observation), DistanceFunction(my.samples[[i]][2, , ], my.observation))
  #       weights[i] <- sum(temp < my.epsilon)
  #     }
  #     return(weights)
}


CalculateWeights <-
  function(old.weight, inclusion.sum.nominator, inclusion.sum.denominator) {
    old.weight * (inclusion.sum.nominator / inclusion.sum.denominator)
  }



