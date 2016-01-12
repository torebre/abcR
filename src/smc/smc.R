# library(NLRoot)

source("smcParameters.R")
source("ess.R")
source("generateParticles.R")
source("calculateWeights.R")
source("distanceFunction.R")



# Sequential Markov Chain Monte Carlo
alpha <- 0.9

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
current.epsilon <- 10
while (T) {
  # Adaptation
  
  nominator <- CalculateInclusionSum(particles, DistanceFunction, current.epsilon, actual.structure)
  
  weights <- CalculateWeights(previous.weights, nominator, previous.inclusion.sum)
  
  ess <- ComputeEffectiveSampleSize(weights)
  
#   current.epsilon <- BFfzero(function(epsilon.candidate) {
#     FindNextEpsilon(epsilon.candidate, particles, previous.ess, previous.inclusion.sum, previous.weights)
#   }, 1, current.epsilon)
  
  
  print(paste("current.epsilon: ", current.epsilon))
  print(paste("weights: ", weights))
  print(paste("ess: ", ess))
  
  print(paste("Value at lower bound: ", FindNextEpsilon(2, particles, previous.ess, previous.inclusion.sum, previous.weights)))
  
  current.epsilon <- uniroot(function(epsilon.candidate) {
    FindNextEpsilon(epsilon.candidate, particles, previous.ess, previous.inclusion.sum, previous.weights)
  }, c(1, current.epsilon))$root
  
  # Resampling
  
  
  # Mutation
  
  previous.weights <- weights
  previous.inclusion.sum <- nominator
  
  counter <- counter + 1
  
}



TempFun <- Vectorize(function(epsilon.candidate) { FindNextEpsilon(epsilon.candidate, particles, previous.ess, previous.inclusion.sum, previous.weights) })
curve(TempFun, 0, 5)
