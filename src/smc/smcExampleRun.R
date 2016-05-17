library(MASS)
library(latex2exp)

ma.configuration <- smcMovingAverageExample(use.raw.distance.function = F, create.debug.variables = T)

ma.example.results <-
  Smc(
    ma.configuration,
    max.iterations = 10000,
    alpha = 0.9,
    number.of.particles = 1000,
    number.of.replicates = 1,
    stop.epsilon = 0.01,
    start.epsilon = 10000,
    verbose = T
  )


SampleFunction <- ma.configuration[["SampleFunction"]]
ForwardKernelSample <- ma.configuration[["ForwardKernelSample"]]
DistanceFunction <- ma.configuration[["DistanceFunction"]]
GenerateRandomPrior <- ma.configuration[["GenerateRandomPrior"]]

run.length <- length(ma.example.results[["effective.sample.sizes"]])

state.to.visualise <- run.length

effective.sample.size <- ma.example.results[["effective.sample.sizes"]][state.to.visualise]
epsilon <- ma.example.results[["epsilons"]][state.to.visualise]
thetas <- ma.example.results[["all.thetas"]]
weights <- unlist(ma.example.results[["all.weights"]][state.to.visualise])
particles <- ma.example.results[["all.particles"]][[state.to.visualise]]

dim(thetas[[1]])

theta1 <- 0.6
theta2 <- 0.2
theta1.range <- seq(-1.99, 1.99, length.out = 50)
theta2.range <- seq(-0.99, 0.99, length.out = 50)
time.series.length <- 100

observed.series <- ma.configuration[["observed.series"]]

likelihood.map <- ComputeMALikelihood(time.series.length, observed.series, theta1.range = theta1.range, theta2.range = theta2.range)

# filled.contour(theta1.range, theta2.range, likelihood.map, xlim = c(-2, 2), ylim = c(-1, 1))



thetas.at.step <- thetas[[state.to.visualise]]

filled.contour(theta1.range, theta2.range, likelihood.map,
               plot.axes = points(t(thetas.at.step), pch = 19, cex = 0.1), xlim = c(-2, 2), ylim = c(-1, 1))

my.filled.contour(theta1.range, theta2.range, likelihood.map,
                  plot.axes = {points(t(thetas.at.step), pch = 19, cex = 0.1); axis(1); axis(2)}, xlim = c(-2, 2), ylim = c(-1, 1))
title(xlab = latex2exp("$\\theta_{1}$"), ylab = latex2exp("$\\theta_{2}$"))

# dev.new(width=720, height=240)
animation::saveGIF(
  for(i in seq(1, run.length, by = 1)) {
    thetas.at.step <- thetas[[i]]
    # plot(t(thetas.at.step), xlim = c(-2, 2), ylim = c(-1, 1), pch = 16, cex = 0.1, ann = F)

    # filled.contour(theta1.range, theta2.range, likelihood.map,
    #                plot.axes = points(t(thetas.at.step), pch = 19, cex = 0.1), xlim = c(-2, 2), ylim = c(-1, 1))


    par("mfrow" = c(1, 3))

    plot.new()
    my.filled.contour(theta1.range, theta2.range, likelihood.map,
                      plot.axes = {points(t(thetas.at.step), pch = 19, cex = 0.1); axis(1); axis(2)}, xlim = c(-2, 2), ylim = c(-1, 1))
    title(xlab = latex2exp("$\\theta_{1}$"), ylab = latex2exp("$\\theta_{2}$"))

    PlotEpsilonTrace(ma.example.results, i, use.run.length = run.length)
    PlotEssTrace(ma.example.results, i, use.run.length = run.length)

    # title(main = "Likelihood", xlab = latex2exp("$\\theta_{1}$"), ylab = latex2exp("$\\theta_{2}$"))
  }, movie.name = "/home/student/likelihood_update2.gif", interval = 0.3, ani.width=720, ani.height=240)

