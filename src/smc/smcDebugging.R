

plot(unlist(debug.variables$avg.acc.rate), type = "l")

plot(unlist(debug.variables$avg.acc.rate))

plot(unlist(results$effective.sample.sizes), type = "l")
plot(unlist(debug.variables$empirical.variance), type = "l")
plot(unlist(results$all.particles[length(results$all.particles)]), pch = 16)
plot(unlist(debug.variables$alive.particles), type = "l")
plot(unlist(debug.variables$accepted), type = "l")

plot(unlist(results$all.weights[length(results$all.weights)]))

hist(unlist(results$all.particles[length(results$all.particles)]))
hist(unlist(results$all.particles[1]))

plot(unlist(results$epsilons), type = "l")

for(i in 0:20) {
# hist(unlist(results$all.thetas[length(results$all.thetas)]), breaks = seq(from = -3, to = 3, by = 0.1), freq = F, ylim = c(0, 2.5), ann = F)
hist(unlist(results$all.thetas[length(results$all.thetas) - i]), freq = F, ylim = c(0, 2.5), ann = F)
}

hist(result.thetas)

theta.final <- unlist(results$all.thetas[length(results$all.thetas)])
border <- max(ceiling(abs(max(theta.final))), ceiling(abs(min(theta.final))))

hist(unlist(results$all.thetas[length(results$all.thetas)]), breaks = seq(from = -4, to = 4, by = 0.1), freq = F, ylim = c(0, 2.5), ann = F)

# Prior is uniform, and is not included here
Posterior <- function(my.theta) {
  my.epsilon <- 10
  (pnorm(my.epsilon - my.theta) - pnorm(-(my.epsilon + my.theta)) + pnorm(10 * (my.epsilon - my.theta))) - pnorm(-10 * (my.epsilon + my.theta))
}

VectorizedPosterior <- Vectorize(Posterior)
curve(VectorizedPosterior,
      from = -5,
      to = 5)

weights.final <- results$all.weights[[length(results$all.weights)]]
# thetas.final <- results$all.thetas[[length(results$all.thetas)]]

hist(thetas.final)

draws <- 200
resampling.indices <- sample(1:kNumberOfParticles, draws, replace = T, prob = weights.final)

my.env$particles <- my.env$particles[resampling.indices]

sample.thetas <- thetas.final[resampling.indices]

plot(sample.thetas)
hist(sample.thetas)

# my.env$thetas <- my.env$thetas[resampling.indices]


# Prior is uniform, and is not included here
Posterior <- function(my.theta) {
  0.5 * dnorm(kObservation, mean = my.theta, sd = 1) + 0.5 * dnorm(kObservation, mean = my.theta, sd = 1 / 10)
}
VectorizedPosterior <- Vectorize(Posterior)
curve(VectorizedPosterior,
      from = -3,
      to = 3,
      add = T)

