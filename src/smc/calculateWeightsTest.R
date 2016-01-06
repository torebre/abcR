source("calculateWeights.R")


DistFunc <- function(my.sample) {
  # Just for testing
  return(1)
}

particles2 <- GenerateParticlesSpatial(10, 1)

weights <- CalculateWeights(particles2, DistFunc, 2)

