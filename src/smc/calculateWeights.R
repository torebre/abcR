

CalculateWeights <- function(my.samples, DistanceFunction, epsilon) {
  weights <- rep(NA, length(my.samples))
  for(i in 1:length(my.samples)) {
    weights[i] <- 
      sum(sapply(my.samples[[i]], function(sample.replicate) {
        DistanceFunction(sample.replicate)
      }) < epsilon)
  }
  return(weights)
}

