
Posterior2 <- function(my.theta) {
  0.5 * dnorm(0, mean = my.theta, sd = 1) + 0.5 * dnorm(0, mean = my.theta, sd = 1 / 10)
}

VectorizedPosterior2 <- Vectorize(Posterior2)

hist(
  unlist(thetas),
  freq = F,
  ann = F,
  xlim = c(-3, 3),
  ylim = c(0, 2.5)
)
title(latex2exp::latex2exp("$\\theta$"))

curve(
  VectorizedPosterior2,
  from = -3,
  to = 3,
  add = T,
  col = "green"
)


RunExperiment1(alpha.to.use = 0.9, number.of.particles.to.use = 1000, file.name.prefix = "exp1_09_1000")
RunExperiment1(alpha.to.use = 0.9, number.of.particles.to.use = 10000, file.name.prefix = "exp1_09_10000")
RunExperiment1(alpha.to.use = 0.9, number.of.particles.to.use = 100000, file.name.prefix = "exp1_09_100000")

