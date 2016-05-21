toy.example.configuration.1 <- smcToyExample(create.debug.variables = T)
toy.example.results.1 <-
  Smc(
    toy.example.configuration.1,
    max.iterations = 1000,
    alpha = 0.9,
    number.of.particles = 1000,
    number.of.replicates = 5,
    stop.epsilon = 0.01,
    verbose = T
  )


state.to.visualise <- 150

DistanceFunction <- smc.configuration[["DistanceFunction"]]

effective.sample.size <- smc.configuration[["effective.sample.sizes"]][state.to.visualise]
epsilon <- unlist(smc.configuration[["epsilons"]][state.to.visualise])
thetas <- unlist(smc.configuration[["all.thetas"]][state.to.visualise])
weights <- unlist(smc.configuration[["all.weights"]][state.to.visualise])
particles <- smc.configuration[["all.particles"]][[state.to.visualise]]


plot(weights)


# epsilon.candidate,
# my.current.epsilon,
# my.particles,
# my.previous.weights,
# alpha,
# DistanceFunction

current.epsilon <- unlist(epsilon)

weights <- variable.env$weights
particles <- variable.env$particles
thetas <- variable.env$thetas
epsilons <- variable.env$epsilons
ess <- variable.env$effective.sample.sizes
all.weights <- variable.env$all.weights

# weights <- my.previous.weights
# particles <- my.particles
# current.epsilon <- my.current.epsilon

# weights <- unlist(smc.configuration[["all.weights"]][state.to.visualise - 1])

temp.function <- Vectorize(function(x) {FindNextEpsilon(x, current.epsilon, particles, weights, 0.9, DistanceFunction)})
curve(temp.function, from = 0, to = current.epsilon, ann = F) # ylim = c(-10, 100))
title(xlab = latex2exp("$\\epsilon_{candidate}$"))
abline(v = new.epslion, col = "red")

use.thetas <- c(-3, 3)

run.length <- length(epsilons)

PlotHistogram(thetas, use.thetas)
# PlotEpsilonTrace(toy.example.results.1, state.to.visualise, use.run.length = run.length)
# PlotEssTrace(toy.example.results.1, state.to.visualise, use.run.length = run.length)

PlotEpsilonTrace(unlist(epsilons), state.to.visualise, use.run.length = run.length)
PlotEssTrace(ess, state.to.visualise, use.run.length = run.length)


alpha <- 0.9

new.epslion <- uniroot(function(epsilon.candidate) {
  FindNextEpsilon(
    epsilon.candidate,
    current.epsilon,
    particles,
    weights,
    alpha,
    DistanceFunction
  )
}, interval = c(0, current.epsilon), extendInt = "no", check.conv = T)$root




new.epslion2 <- uniroot(function(epsilon.candidate) {
  FindNextEpsilon2(
    epsilon.candidate,
    current.epsilon,
    particles,
    weights,
    alpha,
    DistanceFunction
  )
}, interval = c(0, current.epsilon), extendInt = "no", check.conv = T)$root
