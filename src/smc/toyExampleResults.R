library(MASS)
library(latex2exp)

# Prior is uniform, and is not included here
Posterior <- function(my.theta) {
  0.5 * dnorm(kObservation, mean = my.theta, sd = 1) + 0.5 * dnorm(kObservation, mean = my.theta, sd = 1 / 10)
}
VectorizedPosterior <- Vectorize(Posterior)
curve(VectorizedPosterior, from = -3, to = 3)


particles.vector <- sapply(particles, function(x) {x[1]})
particles.sample <- sample(particles.vector, kNumberOfParticles, replace = T, weights)

hist(thetas, breaks = seq(from = -10, to = 10, by = 0.1), freq = F, ylim = c(0, 2.5), ann = F)
curve(VectorizedPosterior, from = -10, to = 10, add = T, col = "red")
title(main = "Density", xlab = latex2exp("$p(\\theta | y)$"))

# truehist(thetas, xlim = c(-3, 3), ylim = c(0, 3), ann = F)
# curve(VectorizedPosterior, from = -10, to = 10, add = T, col = "red")
# title(xlab = latex2exp("$p(\\theta | y)$"))


# temp <- sapply(epsilons, function(x) {x[1]})
# dim(temp) <- NULL
# temp <- vector(epsilons)

plot(unlist(epsilons)[2:length(epsilons)], type = "l", ann = F)
title(main = latex2exp("$\\epsilon_{n}$"), ylab = latex2exp("$\\epsilon_{n}$"), xlab = "n")


plot(unlist(effective.sample.sizes), type = "l", ylim = c(0, 1000), ann = F)
abline(h = 500)
title(main = "Effectice sample size", xlab = "n", ylab = "EFF")

index.to.check <- which.max(thetas)


# particle.history <- rep(NA, length())

theta.history <- rep(NA, length(all.thetas))
for(i in 1:length(all.thetas)) {
  theta.history[i] <- all.thetas[[i]][index.to.check]
}
plot(theta.history, type = "l", ann = F)
title(main = latex2exp(paste("$\\theta$ for index ", index.to.check)))


weight.history <- rep(NA, length(all.weights))
for(i in 1:length(all.weights)) {
  weight.history[i] <- all.weights[[i]][index.to.check]
}
plot(weight.history, type = "l")
title(main = paste("Weight for index ", index.to.check))


particle.history <- rep(NA, length(all.particles))
for(i in 1:length(all.particles)) {
  particle.history[i] <- all.particles[[i]][index.to.check]
}
plot(particle.history, type = "l", ann = F)
title(main = paste("Drawn sample for index ", index.to.check))


