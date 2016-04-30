toy.example.configuration.2 <- smcToyExample(create.debug.variables = T)

toy.example.results.2 <-
  Smc(
    toy.example.configuration.2,
    max.iterations = 1000,
    alpha = 0.95,
    number.of.particles = 10000,
    number.of.replicates = 1,
    stop.epsilon = 0.01,
    verbose = T
  )

toy.example.configuration.3 <- smcToyExample(create.debug.variables = T)
toy.example.results.3 <-
  Smc(
    toy.example.configuration.3,
    max.iterations = 100000,
    alpha = 0.95,
    number.of.particles = 10000,
    number.of.replicates = 5,
    stop.epsilon = 0.01,
    verbose = T
  )



debug.variables.environment <- toy.example.configuration.2[["GetDebugVariables"]]()


state.to.visualise <- length(toy.example.results.2[["effective.sample.sizes"]])

effective.sample.size <-
  toy.example.results.2[["effective.sample.sizes"]][state.to.visualise]
epsilon <- toy.example.results.2[["epsilons"]][state.to.visualise]
thetas <- unlist(toy.example.results.2[["all.thetas"]][state.to.visualise])
weights <- unlist(toy.example.results.2[["all.weights"]][state.to.visualise])
particles <- toy.example.results.2[["all.particles"]][[state.to.visualise]]

use.thetas <- c(-3, 3)


png("experiment2_histogram.png") #, width = 480, height = 480 * 3/2)
PlotHistogram(thetas, use.thetas)
dev.off();

# PlotParticles(particles, weights, use.max = distance.max)
PlotEpsilonTrace(smc.resultoy.example.results.2, state.to.visualise, use.run.length = run.length)
PlotEssTrace(toy.example.results.2, state.to.visualise, use.run.length = run.length)

# CreateGifAnimation(toy.example.results.2, "toy_example_results_2.gif")



plot(avg.acc.rate, type = "l", col = "red")
lines(avg.acc.rate2, add = T, col = "blue")






