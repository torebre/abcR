library(latex2exp)
library(MASS)

state.to.visualise = run.length

# png("/home/student/ma_aut_09.png")
# png("/home/student/ma_raw_dist_09.png")
# png("/home/student/ma_raw_dist_99.png")

# op <- par(no.readonly = TRUE)

layout(matrix(c(0, 1, 1, 0, 2, 2, 3, 3), 2, 4, byrow = TRUE), respect = F)
thetas.at.step <- thetas[[run.length]]

plot.new()
my.filled.contour(
  theta1.range,
  theta2.range,
  likelihood.map,
  plot.axes = {
    points(t(thetas.at.step), pch = 19, cex = 0.1)
    axis(1)
    axis(2)
  },
  xlim = c(-2, 2),
  ylim = c(-1, 1)
)
title(xlab = latex2exp("$\\theta_{1}$"),
      ylab = latex2exp("$\\theta_{2}$"))

plot.new()
my.filled.contour(
  theta1.range,
  theta2.range,
  likelihood.map,
  plot.axes = {
    axis(1)
    axis(2)
  },
  xlim = c(-2, 2),
  ylim = c(-1, 1)
)
title(xlab = latex2exp("$\\theta_{1}$"),
      ylab = latex2exp("$\\theta_{2}$"))


TempDistanceFunction <-
  function(x) {
    ma.configuration$DistanceFunction(unlist(x))
  }
DistanceFunctionVectorized <- Vectorize(TempDistanceFunction)

run.length <- length(ma.example.results[["effective.sample.sizes"]])
particle.max <-
  max(abs(unlist(ma.example.results[["all.particles"]])))

animation::saveVideo(for (i in seq(1, run.length, by = 1)) {
  # op <- par(mfrow = c(2, 2))
  layout(matrix(
    c(1, 1, 2, 2, 3, 3, 0, 4, 4, 5, 5, 0),
    nrow = 2,
    ncol = 6,
    byrow = TRUE
  ), respect = F)
  thetas.at.step <- ma.example.results[["all.thetas"]][i][[1]]

  # Plot with particles as dots superimposed on the likelihood
  plot.new()
  my.filled.contour(
    theta1.range,
    theta2.range,
    likelihood.map,
    plot.axes = {
      points(t(thetas.at.step), pch = 19, cex = 0.1)
      axis(1)
      axis(2)
    },
    xlim = c(-2, 2),
    ylim = c(-1, 1)
  )
  title(xlab = latex2exp("$\\theta_{1}$"),
        ylab = latex2exp("$\\theta_{2}$"))

  # Plot heat map of thetas
  temp <- unlist(thetas.at.step)
  k <-
    kde2d(temp[1,], temp[2,], n = 200, lims = c(c(-2, 2), c(-1, 1)))
  image(k, col = cm.colors(20))
  lines(c(0, 2), c(-1, 1))
  lines(c(0,-2), c(-1, 1))
  title(xlab = latex2exp("$\\theta_{1}$"),
        ylab = latex2exp("$\\theta_{2}$"))

  particle.vector <- ma.example.results[["all.particles"]][[i]]
  distances <-
    sapply(particle.vector, function(x) {
      ma.configuration$DistanceFunction(unlist(x))
    })
  plot(
    1:length(distances),
    distances,
    pch = 16,
    cex = 0.2,
    ylim = c(0, 20), ann = F
  )
  title(xlab = "Particle index", ylab = "Distance")

  abline(h = ma.example.results[["epsilons"]][[i]])

  PlotEpsilonTrace(
    ma.example.results,
    state.to.visualise = i,
    use.run.length = run.length,
    use.eps.max = 20
  )

  PlotEssTrace(ma.example.results,
               state.to.visualise = i,
               use.run.length = run.length)

},
video.name = "ma_autocovariance.mp4",
ffmpeg = "avconv",
interval = 0.3)

dev.off()

# png("/home/student/ma_raw_dist_09_2.png", width = 480, height = 240)
#png("/home/student/ma_aut_09_2.png", width = 480, height = 240)
# png("/home/student/ma_raw_dist_099_2.png", width = 480, height = 240)
layout(matrix(c(1, 2), 1, 2, byrow = TRUE), respect = F)
PlotEpsilonTrace(
  ma.example.results,
  state.to.visualise = state.to.visualise,
  use.run.length = run.length,
  use.eps.max = 1000
)
PlotEssTrace(ma.example.results,
             state.to.visualise = state.to.visualise,
             use.run.length = run.length)
dev.off()

# par(op)
