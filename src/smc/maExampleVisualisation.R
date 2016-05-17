library(latex2exp)

ma.example.configuration3 <- smcMovingAverageExample(use.raw.distance.function = F, create.debug.variables = T)

run.length <- length(ma.example.results3$all.thetas)
all.particles <- ma.example.results3$all.particles
all.thetas <- ma.example.results3$all.thetas

GenerateSample <- ma.example.configuration3[["GenerateSample"]]
time.series <- unlist(GenerateSample(c(theta1, theta2)))
plot(time.series, type = "l")

dim(time.series)

png("test_time_series_plot.png")

first <- T
counter <- 1
for(time.series in all.particles[[run.length]]) {
  # print(dim(unlist(time.series)))

  if(first) {
    plot(unlist(time.series)[1, ], type = "l")
    first <- F
  }
  points(unlist(time.series)[1, ], type = "l")

  # if(counter > 10) {
  # break
  # }
  # counter <- counter + 1
}

dev.off()

theta1 <- theta1.actual
theta2 <- theta2.actual
time.series.length <- 100
sigma.squared <- 1

theta1.range <- seq(-2, 2, 0.2)
theta2.range <- seq(-1, 1, 0.2)
time.series.length <- 100
observed.series <- ma.configuration[["observed.series"]]

likelihood.map <- ComputeMALikelihood(theta1, theta2, time.series.length, time.series = observed.series)


# TODO Fix problems with directions
filled.contour(theta1.range, theta2.range, likelihood.map)

animation::saveGIF(
  for(i in seq(1, run.length, by = 10)) {
    thetas.at.step <- all.thetas[[i]]
    # plot(t(thetas.at.step), xlim = c(-2, 2), ylim = c(-1, 1), pch = 16, cex = 0.1, ann = F)

    filled.contour(theta1.range, theta2.range, t(likelihood.map),
                   plot.axes = points(t(thetas.at.step), pch = 19, cex = 0.1))

    title(main = "Likelihood", xlab = latex2exp("$\\theta_{1}$"), ylab = latex2exp("$\\theta_{2}$"))
  }, movie.name = "/home/student/likelihood_update.gif", interval = 0.3)


