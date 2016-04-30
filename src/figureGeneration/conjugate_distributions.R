library(latex2exp)


n <- 10
p <- 0.3
y <- 3

alpha <- 2
beta <- 4


VectorizedPrior <- Vectorize(function(theta) { dbeta(theta, alpha, beta) })
curve(VectorizedPrior, col = "blue", ann = F)
title(main = latex2exp("$p(\\theta)$"))

VectorizedLikelihood <- Vectorize(function(theta) { dbinom(y, n, theta) })
curve(VectorizedLikelihood, col = "blue", ann = F)
title(main = latex2exp("$p(y|\\theta)$ as function of $\\theta$"), xlab = latex2exp("$\\theta$"))

VectorizedPosterior <- Vectorize(function(theta) { dbeta(theta, alpha + y, beta + n - y) })
curve(VectorizedPosterior, col = "blue", ann = F)
title(main = latex2exp("$cp(\\theta |y)$"), xlab = latex2exp("$\\theta$"))

x.values <- seq(0, 1, 0.05)
plot(c(0, 1), c(0, 3), type = "n")

prior.color <- rgb(0, 1, 0, 0.5)
y.values2 <- sapply(x.values, VectorizedPrior)
polygon(c(x.values, rev(x.values)), c(y.values2, rep(0, length(y.values2))), col = prior.color)

likelihood.color <- rgb(0, 0, 1, 0.5)
y.values3 <- sapply(x.values, VectorizedLikelihood)
polygon(c(x.values, rev(x.values)), c(y.values3, rep(0, length(y.values3))), col = likelihood.color)


plot(c(0, 1), c(0, 3), type = "n")
y.values <- sapply(x.values, VectorizedPosterior)
posterior.color <- rgb(1, 0, 0)
polygon(c(x.values, rev(x.values)), c(y.values, rep(0, length(y.values))), col = posterior.color)



plot(c(0, 1), c(0, 3), type = "n")
y.values.test <- sapply(x.values, function(x) {VectorizedPrior(x) *  VectorizedLikelihood(x)})
posterior.color <- rgb(1, 0, 0)
polygon(c(x.values, rev(x.values)), c(y.values.test, rep(0, length(y.values.test))), col = posterior.color)


plot(c(0, 1), c(0, 3), type = "n")
y.values.test2 <- sapply(x.values, function(x) { (gamma(n + alpha + beta) / (gamma(y + alpha) * gamma(n - y + beta))) * x^(y + alpha - 1) * (1 - x)^(n - y + beta - 1) })
posterior.color <- rgb(1, 0, 0)
polygon(c(x.values, rev(x.values)), c(y.values.test2, rep(0, length(y.values.test2))), col = posterior.color)



# conditionalStdDev = sqrt(diag(sigma))
# lowerBorder = mu - 2 * conditionalStdDev
# upperBorder = mu + 2 * conditionalStdDev
# polygonUpper = cbind(1:47, upperBorder)
# polygonLower = cbind(47:1, rev(lowerBorder))
# polygon(c(polygonUpper, polygonLower))
# vertices = rbind(polygonUpper, polygonLower)
# plot(mu, ylim = c(min(vertices[, 2]), max(vertices[, 2])), type = "l")
# polygon(vertices[, 1], vertices[, 2], col = "grey90", border = NA)
# lines(mu)

