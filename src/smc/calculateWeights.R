


CalculateInclusionSum <-
  function(my.samples, DistanceFunction, my.epsilon, my.observation) {
    weights <- rep(NA, length(my.samples))
    for (i in 1:length(my.samples)) {
      
      temp <- c(DistanceFunction(my.samples[[i]][1, , ], my.observation), DistanceFunction(my.samples[[i]][2, , ], my.observation))
      
#       temp <- sapply(my.samples[[i]], function(sample.replicate) {
#         DistanceFunction(sample.replicate, my.observation)
#       })
      
      
      # print(paste("my.epsilon2: ", my.epsilon))
      
      weights[i] <- sum(temp < my.epsilon)
    }
    return(weights)
  }


CalculateWeights <-
  function(old.weight, inclusion.sum.nominator, inclusion.sum.denominator) {
    old.weight * (inclusion.sum.nominator / inclusion.sum.denominator)
  }