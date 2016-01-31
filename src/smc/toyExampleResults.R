library(MASS)

# Prior is uniform, and is not included here
Posterior <- function(my.theta) {
  0.5 * dnorm(kObservation, mean = my.theta, sd = 1) + 0.5 * dnorm(kObservation, mean = my.theta, sd = 1 / 10)
}
VectorizedPosterior <- Vectorize(Posterior)
curve(VectorizedPosterior, from = -3, to = 3)


particles.vector <- sapply(particles, function(x) {x[1]})
particles.sample <- sample(particles.vector, kNumberOfParticles, replace = T, weights)





truehist(thetas)
curve(VectorizedPosterior, from = -10, to = 10, add = T)


