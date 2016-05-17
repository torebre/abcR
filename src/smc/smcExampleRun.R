library(MASS)

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

thetas.at.step <- thetas[[1]]

filled.contour(theta1.range, theta2.range, likelihood.map,
               plot.axes = points(t(thetas.at.step), pch = 19, cex = 0.1), xlim = c(-2, 2), ylim = c(-1, 1))

my.filled.contour(theta1.range, theta2.range, likelihood.map,
               plot.axes = {points(t(thetas.at.step), pch = 19, cex = 0.1); axis(1); axis(2)}, xlim = c(-2, 2), ylim = c(-1, 1))


zlim = range(likelihood.map, finite = TRUE)
nlevels <- 20
color.palette = cm.colors

plot.new()
plot.window(xlim = c(-2, 2), ylim = c(-1, 1))
.filled.contour(theta1.range, theta2.range, likelihood.map, levels = pretty(zlim, nlevels), col = color.palette(length(levels) - 1))

levelplot(theta1.range, theta2.range, likelihood.map,
               plot.axes = points(t(thetas.at.step), pch = 19, cex = 0.1), xlim = c(-2, 2), ylim = c(-1, 1))

grid <- expand.grid(X=theta1.range, Y=theta2.range)
temp <- matrix(likelihood.map, ncol = 2)
grid$Z <-
levelplot(z ~x*y, data = likelihood.map)

image(likelihood.map)

heatmap(likelihood.map)

op <- par("mfrow" = c(2, 2))

theta1 <- 0.6
theta2 <- 0.2
theta1.range <- seq(-1.99, 1.99, length.out = 50)
theta2.range <- seq(-0.99, 0.99, length.out = 50)
time.series.length <- 100

observed.series <- ma.configuration[["observed.series"]]

likelihood.map <- ComputeMALikelihood(time.series.length, observed.series, theta1.range = theta1.range, theta2.range = theta2.range)

# TODO Fix problems with directions
filled.contour(theta1.range, theta2.range, likelihood.map, xlim = c(-2, 2), ylim = c(-1, 1))




animation::saveGIF(
  for(i in seq(1, run.length, by = 10)) {
    thetas.at.step <- all.thetas[[i]]
    # plot(t(thetas.at.step), xlim = c(-2, 2), ylim = c(-1, 1), pch = 16, cex = 0.1, ann = F)

    filled.contour(theta1.range, theta2.range, likelihood.map,
                   plot.axes = points(t(thetas.at.step), pch = 19, cex = 0.1), xlim = c(-2, 2), ylim = c(-1, 1))

    PlotEpsilonTrace(ma.example.results, state.to.visualise, use.run.length = run.length)
    PlotEssTrace(ma.example.results, state.to.visualise, use.run.length = run.length)

    title(main = "Likelihood", xlab = latex2exp("$\\theta_{1}$"), ylab = latex2exp("$\\theta_{2}$"))
  }, movie.name = "/home/student/likelihood_update.gif", interval = 0.3)

