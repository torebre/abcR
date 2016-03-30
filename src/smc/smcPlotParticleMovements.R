library(animation)
library(latex2exp)


run.length <- length(results$epsilons)
particle.max <- max(results$all.particles[[1]])
number.of.particles <- length(results$all.particles[[1]])
sector.size <- 2 * pi / number.of.particles

theta.upper <- -10
theta.lower <- 10

for(i in 1:run.length) {
  theta.upper <- max(theta.upper, unlist(results$all.thetas[i]))
  theta.lower <- min(theta.lower, unlist(results$all.thetas[i]))
}


Posterior2 <- function(my.theta) {
  0.5 * dnorm(0, mean = my.theta, sd = 1) + 0.5 * dnorm(0, mean = my.theta, sd = 1 / 10)
}
VectorizedPosterior2 <- Vectorize(Posterior2)


saveGIF(
for(j in seq(1, run.length, by = 2)) {
  par(mfrow = c(2, 2))
particles.final <- unlist(results$all.particles[j])
epsilon.final <- results$epsilons[j]
weights <- unlist(results$all.weights[j])

# hist(unlist(results$all.thetas[j]), freq = F, ylim = c(0, 2.5), breaks = seq(from = theta.lower, to = theta.upper, by = 0.1), ann = F)
truehist(unlist(results$all.thetas[j]), h = 0.1, xlim = c(-5, 5), ylim = c(0, 2), ann = F)
title(latex2exp("$\\theta$"))

print(paste("epsilon: ", results$epsilons[[j]]))

# Posterior <- function(my.theta) {
#   my.epsilon <- results$epsilons[[j]]
#   (pnorm(my.epsilon - my.theta) - pnorm(-(my.epsilon + my.theta)) + pnorm(10 * (my.epsilon - my.theta))) - pnorm(-10 * (my.epsilon + my.theta))
# }
# VectorizedPosteror <- Vectorize(Posterior)
# curve(VectorizedPosterior,
#       from = -5,
#       to = 5, add = T)

curve(VectorizedPosterior2,
      from = -5,
      to = 5, add = T, col = "green")


# Plot particles
cumulative.angle <- 0
x.coords <- rep(NA, number.of.particles)
y.coords <- rep(NA, number.of.particles)
colors <- rep(NA, number.of.particles)
for(i in 1:number.of.particles) {
  particle.distance <- particles.final[i]
  x.coords[i] <- cos(cumulative.angle) * particle.distance
  y.coords[i] <- sin(cumulative.angle) * particle.distance
  cumulative.angle <- cumulative.angle + sector.size

  if(weights[i] > 0) {
    colors[i] <- "red"
  }
  else {
    colors[i] <- "blue"
  }
}

plot(x.coords, y.coords, pch = 16, cex = 0.1, xlim = c(-particle.max, particle.max), ylim = c(-particle.max, particle.max), col = colors, ann = F)
title("d(x, 0)")


plot(unlist(results$epsilons)[1:j], type = "l", ann = F, xlim = c(0, run.length), ylim = c(0, 10))
title(main = latex2exp("$\\epsilon_{n}$"), ylab = latex2exp("$\\epsilon_{n}$"), xlab = "n")


plot(unlist(results$effective.sample.sizes)[1:j], type = "l", ylim = c(0, 10000), ann = F, xlim = c(0, run.length))
abline(h = 5000)
title(main = "ESS", xlab = "n", ylab = "EFF")



# draw.circle(0, 0, epsilon.final)

Sys.sleep(0.5)

}, movie.name = "smc_toy_example.gif", interval = 0.3)

