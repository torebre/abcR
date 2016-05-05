toy.example.configuration.2 <-
  smcToyExample(create.debug.variables = T)

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

toy.example.configuration.3 <-
  smcToyExample(create.debug.variables = T)
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


CreatePlotForReplicateExperiment <-
  function(plot.prefix,
           example.configuration,
           example.results) {
    debug.variables.environment <-
      example.configuration[["GetDebugVariables"]]()

    state.to.visualise <-
      length(example.results[["effective.sample.sizes"]])

    run.length <- state.to.visualise

    effective.sample.size <-
      example.results[["effective.sample.sizes"]][state.to.visualise]
    epsilon <- example.results[["epsilons"]][state.to.visualise]
    thetas <-
      unlist(example.results[["all.thetas"]][state.to.visualise])
    weights <-
      unlist(example.results[["all.weights"]][state.to.visualise])
    particles <-
      example.results[["all.particles"]][[state.to.visualise]]

    use.thetas <- c(-3, 3)

    avg.acc.rate <- unlist(debug.variables.environment$avg.acc.rate)

    png(paste(plot.prefix, ".png")) #, width = 480, height = 480 * 3/2)
    par(mfrow = c(2, 2))
    plot(avg.acc.rate, type = "l")
    PlotHistogram(thetas, use.thetas)
    PlotEpsilonTrace(example.results, state.to.visualise, use.run.length = run.length)
    PlotEssTrace(example.results, state.to.visualise, use.run.length = run.length)
    dev.off()

    # PlotParticles(particles, weights, use.max = distance.max)
    # CreateGifAnimation(toy.example.results.2, "toy_example_results_2.gif")

  }

CreatePlotForReplicateExperiment("configuration3_5_replicates", toy.example.configuration.3, toy.example.results.3)

