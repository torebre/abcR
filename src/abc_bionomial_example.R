# Implementing a toy ABC example seen in 
# A tutorial on approximate Bayesian computation
# Brandon M. Turner, Trisha Van Zandt
library(MASS)

# The setup is that there is a binomial experiment where
# the pareameter is assumed to have a beta-distribution.
# In this case the posterior is also a beta distribution

kSuccessProb <- 0.7
kTolerance <- 0.0

number.of.observations <- 100
number.of.correct.responses <- number.of.observations * kSuccessProb

prior.alpha <- 7
prior.beta <- 3

DistanceFunction <- function(x, y, my.number.of.observations) {
  (1 / my.number.of.observations) * abs(x - y)
}

abc.success.estimate <- rep(NA, number.of.observations)

for (i in 1:number.of.observations) {
  repeat {
    prior.sample <- rbeta(1, prior.alpha, prior.beta)
    samples <-
      rbinom(number.of.observations, size = 1, prob = prior.sample)
    number.of.correct.responses.in.samples <- sum(samples)
    if (DistanceFunction(number.of.correct.responses.in.samples, number.of.correct.responses, number.of.observations) <= kTolerance) {
      print(paste('Sample: ', prior.sample))
      abc.success.estimate[i] <- prior.sample
      break;
    }
  }
}

truehist(abc.success.estimate, nbins = 10)

TruePosteriorDensity = Vectorize(function(x) {
  dbeta(
    x, number.of.correct.responses + prior.alpha,
    number.of.observations - number.of.correct.responses + prior.beta
  )
})
curve(TruePosteriorDensity, add = T)
