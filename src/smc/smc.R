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
thetas <- sapply(1:kNumberOfParticles, function(x) { GenerateRandomSampleFromTheta() })
particles <- GenerateParticles(thetas, kNumberOfReplicates)

FindNextEpsilon <- function(epsilon.candidate, my.current.epsilon, my.particles, my.previous.weights) {
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


counter <- 0
resample.limit <- kNumberOfParticles * kResampleRatio


# CalculateWeightUpdateForParticle <- function(my.samples, my.particle.number, my.old.epsilon, my.new.epsilon, DistanceFunction)

# CalculateInclusionSum <- function(my.sample.replicates, DistanceFunction, my.epsilon) {



# previous.inclusion.sum <- CalculateInclusionSum(samples, DistanceFunction, current.epsilon, kObservation)
weights <- rep(1 / kNumberOfParticles, kNumberOfParticles)
# previous.effective.sample.size <- ComputeEffectiveSampleSize(previous.weights)
effective.sample.size <- kNumberOfParticles
current.epsilon <- 1000000

temp.function <- Vectorize(function(x) {FindNextEpsilon(x, current.epsilon, particles, weights)})
curve(temp.function, from = 1, to = 15)

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
  
  if(current.epsilon <= kStopEpsilon) {
    break
  }
  
  # Find next epsilon
  epsilon.new <- uniroot(function(epsilon.candidate) {
    FindNextEpsilon(epsilon.candidate, current.epsilon, particles, weights)
  }, extendInt = "no", c(0, current.epsilon))$root
  
  # TODO Only compute the weights once, not here and in FindNextEpsilon
  weights <- NormalizeVector(weights * CalculateWeightUpdates(particles, current.epsilon, epsilon.new, DistanceFunction))
  if(epsilon.new < current.epsilon) {
    current.epsilon <- epsilon.new
  }
  
  effective.sample.size <- ComputeEffectiveSampleSize(weights)
  
  # Resampling
  if(effective.sample.size < kResampleRatio * kNumberOfParticles) {
    
    print("Resampling")
    resampling.indices <- sample(1:kNumberOfParticles, kNumberOfParticles, replace = T, prob = weights)
    particles <- particles[resampling.indices]
    thetas <- thetas[resampling.indices]
    weights <- rep(1 / kNumberOfParticles, kNumberOfParticles)
  }
  
  # Mutation
  
#   print("Thetas:")
#   print(thetas)
  
  iterated.samples <- ForwardKernelSample(particles, thetas, current.epsilon, weights)
  particles <- iterated.samples$samples
  thetas <- iterated.samples$theta
  
#   print("Thetas2:")
#   print(thetas)
  
  
  counter <- counter + 1
  
  print(paste("Counter: ", counter, " Effective sample size: ", effective.sample.size))
  
  # Just here for testing
#   if(counter == 10) {
#     break;
#   }
  
}




