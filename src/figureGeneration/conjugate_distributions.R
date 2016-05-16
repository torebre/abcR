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


png("conjugate_distributions.png", width = 720, height = 240)
par(mfrow = c(1, 3))

# Setup empty plot
x.values <- seq(0, 1, 0.0025)
plot(c(0, 1), c(0, 3), type= "n", ann = F)
title(xlab = latex2exp("$\\theta$"))

prior.color <- rgb(0, 1, 0, 0.5)
y.values2 <- sapply(x.values, VectorizedPrior)
polygon(c(x.values, rev(x.values)), c(y.values2, rep(0, length(y.values2))), col = prior.color)

likelihood.color <- rgb(0, 0, 1, 3.5)
y.values3 <- sapply(x.values, VectorizedLikelihood)
polygon(c(x.values, rev(x.values)), c(y.values3, rep(0, length(y.values3))), col = likelihood.color)

text = c(latex2exp("$p(x|\\theta)$"), latex2exp("$p(\\theta)$"))
legend("topright", legend = text, lty = c(1, 1), lwd = c(2.5, 2.5), col = c(likelihood.color, prior.color))


# The normalization constant is 0.1598402

plot(c(0, 1), c(0, 3.5), type = "n", ann = F)
y.values.test <- sapply(x.values, function(x) {(1 / 0.1598402) * VectorizedPrior(x) *  VectorizedLikelihood(x)})
posterior.color <- rgb(1, 0, 0)
polygon(c(x.values, rev(x.values)), c(y.values.test, rep(0, length(y.values.test))), col = rgb(0, 0, 1))
title(xlab = latex2exp("$\\theta$"))

text = latex2exp("$\\frac{p(x|\\theta)\\times p(\\theta)}{0.1598402}")
legend("topright", legend = text, lty = c(1, 1), lwd = c(2.5, 2.5), col = rgb(0, 0, 1), adj = c(0, 0.3))


plot(c(0, 1), c(0, 3.5), type = "n", ann = F)
y.values <- sapply(x.values, VectorizedPosterior)
posterior.color <- rgb(1, 1, 0)
polygon(c(x.values, rev(x.values)), c(y.values, rep(0, length(y.values))), col = posterior.color)
title(xlab = latex2exp("$\\theta$"))

text = latex2exp("$p(\\theta | x)$")
legend("topright", legend = text, lty = c(1, 1), lwd = c(2.5, 2.5), col = posterior.color)

dev.off()

plot(c(0, 1), c(0, 3), type = "n")
y.values.test2 <- sapply(x.values, function(x) { (gamma(n + alpha + beta) / (gamma(y + alpha) * gamma(n - y + beta))) * x^(y + alpha - 1) * (1 - x)^(n - y + beta - 1) })
posterior.color <- rgb(1, 0, 0)
polygon(c(x.values, rev(x.values)), c(y.values.test2, rep(0, length(y.values.test2))), col = rgb(0, 0, 1))
