library(NLRoot)

source("smcParameters.R")
source("ess.R")
source("generateParticles.R")
source("calculateWeights.R")
source("distanceFunction.R")



# Sequential Markov Chain Monte Carlo
current.epsilon <- 2
alpha <- 0.5

# Create an initial set of particles
particles <- GenerateParticlesSpatial(kN, kM)


# FindNextEpsilon <-
#   function(epsilon.candidate, my.particles, previous.ess) {
#     ComputeEffectiveSampleSize(NormalizeVector(
#       CalculateWeights(
#         my.particles, DistanceFunction, epsilon.candidate, actual.structure
#       )
#     )) - current.epsilon * previous.ess
#   }


FindNextEpsilon <-
  function(epsilon.candidate, my.particles, previous.ess, previous.inclusion.sum, my.previous.weight) {
    new.ess <- ComputeEffectiveSampleSize(NormalizeVector(CalculateWeights(my.previous.weight, CalculateInclusionSum(my.particles, DistanceFunction, epsilon.candidate, actual.structure), previous.inclusion.sum)))
    new.ess - alpha * previous.ess
    
#     ComputeEffectiveSampleSize(NormalizeVector(
#       CalculateWeights(
#         my.particles, DistanceFunction, epsilon.candidate, actual.structure
#       )
#     )) - my.current.epsilon * previous.ess
  }


counter <- 0
previous.inclusion.sum <- CalculateInclusionSum(particles, DistanceFunction, current.epsilon, actual.structure)
previous.weights <- NormalizeVector(rep(1 / length(particles), length(particles))) # CalculateInclusionSum(particles, DistanceFunction, current.epsilon, actual.structure)

previous.ess <- ComputeEffectiveSampleSize(previous.weights)
  
# ComputeEffectiveSampleSize(NormalizeVector(CalculateWeights(previous.weights, CalculateInclusionSum(particles, DistanceFunction, current.epsilon, actual.structure))))

while (T) {
  # Adaptation
  # weights <- CalculateInclusionSum(particles, DistanceFunction, current.epsilon, actual.structure)
  
#   weights <-
#     CalculateWeights(particles, DistanceFunction, current.epsilon, actual.structure)
#   weights <- NormalizeVector(weights)
  
  # ess <- ComputeEffectiveSampleSize(weights)
  
  
  
  new.epsilon <- BFfzero(function(epsilon.candidate) {
    FindNextEpsilon(epsilon.candidate, particles, previous.ess, previous.inclusion.sum, previous.weights)
  }, 0, current.epsilon)
  
  
  # Resampling
  
  
  # Mutation
  
  
  
  counter <- counter + 1
  
}

