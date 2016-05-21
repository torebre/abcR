
ComputeEffectiveSampleSize2 <- function(weights) {
  1 / sum(weights ^ 2)
}


FindNextEpsilon2 <-
  function(epsilon.candidate,
           my.current.epsilon,
           my.particles,
           my.previous.weights,
           alpha,
           DistanceFunction) {
    ess.old <- ComputeEffectiveSampleSize2(my.previous.weights)
    weight.updates <-
      CalculateWeightUpdates2(my.particles,
                             my.current.epsilon,
                             epsilon.candidate,
                             DistanceFunction)

    print(paste("ESS old:", ess.old))

    if (sum(weight.updates) == 0) {
      return(-alpha * ess.old)
    }
    weights.new <- NormalizeVector2(my.previous.weights * weight.updates)
    # weights.new <- my.previous.weights * weight.updates

    # weights.new <- NormalizeVector(weight.updates)
    ess.new <- ComputeEffectiveSampleSize2(weights.new)

    return(ess.new - alpha * ess.old)
  }

NormalizeVector2 <- function(my.vector) {
  my.vector.sum <- sum(my.vector)
  if(my.vector.sum == 0) {
    return(rep(0, length(my.vector)))
  }
  my.vector / my.vector.sum
}


CalculateWeightUpdates2 <-
  function(my.samples,
           my.old.epsilon,
           my.new.epsilon,
           DistanceFunction) {
    sapply(1:length(my.samples), function(x) {
      CalculateWeightUpdateForParticle2(my.samples,
                                       x,
                                       my.old.epsilon,
                                       my.new.epsilon,
                                       DistanceFunction)
    })
  }

CalculateWeightUpdateForParticle2 <-
  function(my.samples,
           my.particle.number,
           my.old.epsilon,
           my.new.epsilon,
           DistanceFunction) {
    sum1 <-
      CalculateInclusionSum2(my.samples[my.particle.number], DistanceFunction, my.new.epsilon)
    sum2 <-
      CalculateInclusionSum2(my.samples[my.particle.number], DistanceFunction, my.old.epsilon)

    # TODO Just here to see what happens
    # if(sum1 == 0 && sum2 == 0) {
    #   return(1)
    # }

    if (sum1 == 0) {
      return(0)
    }

    if (sum1 == sum2) {
      return(1)
    }

    sum1 / sum2
  }


CalculateInclusionSum2 <-
  function(my.sample.replicates,
           DistanceFunction,
           my.epsilon) {

    # sum(sapply(my.sample.replicates, function(x) {
    #   DistanceFunction(x) <= my.epsilon
    # }))


    # replicates.unlisted <- unlist(my.sample.replicates)

    inclusion.sum <- 0
    for(i in 1:length(my.sample.replicates)) {

      # print(paste("Sample replicates", my.sample.replicates[[i]]))

      if(DistanceFunction(unlist(my.sample.replicates[[i]])) <= my.epsilon) {
        inclusion.sum <- 1 + inclusion.sum
      }

    }
    inclusion.sum
  }

CalculateWeights2 <-
  function(old.weight,
           inclusion.sum.nominator,
           inclusion.sum.denominator) {
    old.weight * (inclusion.sum.nominator / inclusion.sum.denominator)
  }
