 

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