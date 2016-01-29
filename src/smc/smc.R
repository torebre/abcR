
source("smcParameters.R")
source("ess.R")
# source("generateParticles.R")
source("calculateWeights.R")
# source("distanceFunction.R")
# source("forwardKernelSample.R")
source("toyExampleSetup.R")



# Sequential Markov Chain Monte Carlo
alpha <- 0.9

# Create an initial set of particles
thetas <- sapply(1:kNumberOfParticles, function(x) {GenerateRandomSampleFromTheta() })
particles <- GenerateParticles(thetas, kNumberOfReplicates)



# FindNextEpsilon <-
#   function(epsilon.candidate, my.particles, previous.effective.sample.size, previous.inclusion.sum, my.previous.weight) {
#     new.effective.sample.size <- ComputeEffectiveSampleSize(NormalizeVector(CalculateWeights(my.previous.weight, CalculateInclusionSum(my.particles, DistanceFunction, epsilon.candidate, actual.structure), previous.inclusion.sum)))
#     new.effective.sample.size - alpha * previous.effective.sample.size
#   }


FindNextEpsilon <- function(epsilon.candidate, my.current.epsilon, my.particles, my.previous.weights) {
  ess.old <- ComputeEffectiveSampleSize(my.previous.weights)
  weights.new <- my.previous.weights * CalculateWeightUpdates(my.samples, ess.old, epsilon.candidate, DistanceFunction)
  ess.new <- ComputeEffectiveSampleSize(weights.new)
  
  return(ess.new - alpha * ess.old)
  
#     new.effective.sample.size <- ComputeEffectiveSampleSize(NormalizeVector(CalculateWeights(my.previous.weight, CalculateInclusionSum(my.particles, DistanceFunction, epsilon.candidate, actual.structure), previous.inclusion.sum)))
#     new.effective.sample.size - alpha * previous.effective.sample.size
  }



counter <- 0


# CalculateWeightUpdateForParticle <- function(my.samples, my.particle.number, my.old.epsilon, my.new.epsilon, DistanceFunction)

# CalculateInclusionSum <- function(my.sample.replicates, DistanceFunction, my.epsilon) {

# previous.inclusion.sum <- CalculateInclusionSum(samples, L1DistanceFromObservation, current.epsilon, kObservation)
previous.weights <- rep(1 / kNumberOfParticles, kNumberOfParticles)
# previous.effective.sample.size <- ComputeEffectiveSampleSize(previous.weights)
previous.effective.sample.size <- kNumberOfParticles
current.epsilon <- 1000000



while (T) {
  # Adaptation
  weight.updates <- CalculateWeightUpdates(particles, current.epsilon, , L1DistanceFromObservation) {
  
  
  nominator <- CalculateInclusionSum(particles, DistanceFunction, current.epsilon, actual.structure)
  
  weights <- CalculateWeights(previous.weights, nominator, previous.inclusion.sum)
  
  effective.sample.size <- ComputeEffectiveSampleSize(weights)
  
  print(paste("current.epsilon: ", current.epsilon))
  print(paste("weights: ", weights))
  print(paste("effective.sample.size: ", effective.sample.size))
  
  print(paste("Value at lower bound: ", FindNextEpsilon(2, particles, previous.effective.sample.size, previous.inclusion.sum, previous.weights)))
  
  current.epsilon <- uniroot(function(epsilon.candidate) {
    FindNextEpsilon(epsilon.candidate, current.epsilon, particles, previous.weights)
    # FindNextEpsilon(epsilon.candidate, particles, previous.effective.sample.size, previous.inclusion.sum, previous.weights)
  }, c(1, current.epsilon))$root
  
  
  # Resampling
  if(effective.sample.size < kResampleRatio * kNumberOfParticles) {
    # TODO Resample
    
    
  }
  
  
  
  # Mutation
  
  previous.weights <- weights
  previous.inclusion.sum <- nominator
  
  counter <- counter + 1
  
  ForwardKernelSample <- function(sample.old, theta.old, current.epsilon)
  
  
  # Just here for testing
  if(counter == 10) {
    break;
  }
  
}

