

CalculateWeightUpdates <- function(my.samples, my.old.epsilon, my.new.epsilon, DistanceFunction) {
  sapply(1:length(my.samples), function(x) { CalculateWeightUpdateForParticle(my.samples, x, my.old.epsilon, my.new.epsilon, DistanceFunction)})
}

CalculateWeightUpdateForParticle <- function(my.samples, my.particle.number, my.old.epsilon, my.new.epsilon, DistanceFunction) {
  CalculateInclusionSum(my.samples[my.particle.number], DistanceFunction, my.new.epsilon) / CalculateInclusionSum(my.samples[my.particle.number], DistanceFunction, my.old.epsilon)
}


CalculateInclusionSum <- function(my.sample.replicates, DistanceFunction, my.epsilon) {
    sum(sapply(my.sample.replicates, function(x) { DistanceFunction(x) < my.epsilon}))
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