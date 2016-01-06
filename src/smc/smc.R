source("smcParameters.R")
source("ess.R")
source("generateParticles.R")
source("calculateWeights.R")
source("distanceFunction.R")



# Sequential Markov Chain Monte Carlo

current.epsilon <- 2


# Create an initial set of particles
particles <- GenerateParticlesSpatial(kN, kM)


# Adaptation
weights <- CalculateWeights(particles, DistanceFunction, current.epsilon)



# Resampling

ess <- ComputeEffectiveSampleSize(weights)


# Mutation





