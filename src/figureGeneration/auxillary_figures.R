library(MASS)
library(latex2exp)
library(pscl)

# Show example plots for illustrating importance sampling
VectorizedBeta <- Vectorize(function(x) {if(x < 2) { 0 } else {dbeta(x - 2, 2, 5)}})
curve(VectorizedBeta, from = 0, to = 5, ylim = c(0, 4), col = "red", ann = F)

VectorizedNormal <- Vectorize(function(x) {3 * dnorm(x, 2.3, 0.4)})
curve(VectorizedNormal, add = T, col = "blue")

text = c(latex2exp("p(x)"), latex2exp("cg(x)"))
legend("topright", legend = text, lty = c(1, 1), lwd = c(2.5, 2.5), col = c("red", "blue"))

# Show approximations to normal distribution
par(mfrow = c(1, 3))

xlims <- c(-4, 4)
ylims <- c(0, 0.5)
sample.set1 <- rnorm(10)
truehist(sample.set1, ann = F, xlim = xlims, ylim = ylims)
VectorizedNormal2 <- Vectorize(function(x) { dnorm(x)})
curve(VectorizedNormal2, col = "blue", xlim = xlims, ann = F, add = T)
title(sub = "10 samples")

sample.set1 <- rnorm(100)
truehist(sample.set1, ann = F, xlim = xlims, ylim = ylims)
VectorizedNormal2 <- Vectorize(function(x) { dnorm(x)})
curve(VectorizedNormal2, col = "blue", xlim = xlims, ann = F, add = T)
title(sub = "100 samples")

sample.set1 <- rnorm(10000)
truehist(sample.set1, ann = F, xlim = xlims, ylim = ylims)
VectorizedNormal2 <- Vectorize(function(x) { dnorm(x)})
curve(VectorizedNormal2, col = "blue", xlim = xlims, ann = F, add = T)
title(sub = "10000 samples")


par(mfrow = c(1, 1))

alpha <- 1
beta <- 2

VectorizedInverseGamma <- Vectorize(function(x) { densigamma(x, alpha = alpha, beta = beta)})
curve(VectorizedInverseGamma, from = 1, to = 4)

mu <- 3
sd <- 1

samples <- rnorm(10, mu, sd)

VectorizedNormal <- Vectorize(function(x) { dnorm(x, mu, sd)} )
curve(VectorizedNormal, from = 1, to = 4)

Likelihood <- function(x) {
  temp.sum <- 0
  for(i in 1:length(samples)) {
    temp.sum = temp.sum + (samples[i] - mu)^2
  }
  exp(-log(sqrt(2 * pi * x)) - (1/2) * temp.sum / x)
}

PosteriorProp <- function(x) {
  Likelihood(x) * densigamma(x, alpha = alpha, beta = beta)
}

VectorizedLikelihood <- Vectorize(PosteriorProp)
curve(PosteriorProp, from = 1, to = 4)

n = length(samples)
post.alpha <- alpha + n / 2
temp.sum <- 0
for(i in 1:length(samples)) {
  temp.sum = temp.sum + (samples[i] - mu)^2
}
post.beta <- beta + temp.sum / 2

VecorizedPosterior2 <- Vectorize(function(x) { densigamma(x, post.alpha, post.beta) })
curve(VecorizedPosterior2, from = 1, to = 4)
