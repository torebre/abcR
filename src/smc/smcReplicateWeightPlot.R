library(latex2exp)
library(MASS)


DoWeightPlot <- function(resample.ratio, video.name, interval) {
toy.example.configuration <- smcToyExample()
toy.example.results <-
  Smc(
    toy.example.configuration,
    max.iterations = 100000,
    alpha = 0.9,
    number.of.particles = 50,
    number.of.replicates = 10,
    stop.epsilon = 0.01,
    resample.ratio = resample.ratio,
    verbose = T
  )

run.length <- length(toy.example.results[["effective.sample.sizes"]])
particle.max <- max(abs(unlist(toy.example.results[["all.particles"]])))

animation::saveVideo(
  for(i in seq(1, run.length, by = 1)) {
    thetas <- unlist(toy.example.results[["all.thetas"]][i])
    thetas.weights <- unlist(toy.example.results[["all.weights"]][i])
    particles <- toy.example.results[["all.particles"]][[i]]
    epsilon <- unlist(toy.example.results[["epsilons"]][i])

    op <- par("mfrow" = c(2, 2))

    theta.colors <- rep(NA, length(thetas.weights))
    for(j in 1:length(theta.colors)) {
      theta.colors[j] <- hsv(h = 0, s = 1 - thetas.weights[j], v = 1 - thetas.weights[j])
    }
    plot(y = thetas.weights, x = thetas, xlim = c(-3, 3), ylim = c(0, 1), pch = 16, cex = 0.5, col = colors, ann = F)
    title(xlab = latex2exp("$\\theta$"))

    my.number.of.replicates <- length(particles[[1]])
    my.number.of.particles <- length(particles) * my.number.of.replicates
    colors <- rep(NA, my.number.of.particles)
    particle.weights <- rep(NA, my.number.of.particles)

    for(j in 1:length(particles)) {
      for(k in 1:length(particles[[1]])) {
        current.particle.index <- (j - 1) * my.number.of.replicates + k
        colors[current.particle.index] <- hsv(h = 0, s = 1 - thetas.weights[j], v = 1 - thetas.weights[j])
        particle.weights[current.particle.index] <- thetas.weights[j]
      }
    }
    plot(y = particle.weights, x = unlist(particles),
         xlim = c(-particle.max, particle.max), ylim = c(0, 1), pch = 16, cex = 0.5, col = colors, ann = F)
    title(xlab = "x")
    abline(v = -epsilon)
    abline(v = epsilon)

    # abcProject::PlotParticles(particles, weights, cex = 0.3, particle.max, epsilon)

    resampling.indices <- ResampleThetas(length(thetas.weights), thetas.weights)
    PlotThetaHistogram(thetas[resampling.indices], c(-3, 3))

    abcProject::PlotEssTrace(toy.example.results, i, use.run.length = run.length)

    par(op)

    print(paste("i: ", i))

  }, video.name = video.name, ffmpeg = "avconv", interval = interval)

}

ResampleThetas <- function(number.of.samples, thetas.weights) {
  resampling.indices <-
    sample(
      1:number.of.samples,
      number.of.samples,
      replace = T,
      prob = thetas.weights
    )
}

PlotThetaHistogram <- function(thetas, use.thetas) {
  Posterior2 <- function(my.theta) {
    0.5 * dnorm(0, mean = my.theta, sd = 1) + 0.5 * dnorm(0, mean = my.theta, sd = 1 / 10)
  }

  VectorizedPosterior2 <- Vectorize(Posterior2)

  truehist(
    thetas,
    ann = F,
    xlim = c(use.thetas[1], use.thetas[2]),
    ylim = c(0, 2.5)
  )
  title(latex2exp::latex2exp("$\\theta$"))

  curve(
    VectorizedPosterior2,
    from = use.thetas[1],
    to = use.thetas[2],
    add = T,
    col = "green"
  )
}


#DoWeightPlot(0, "weight_plot_resample0.mp4", interval = 0.2)
DoWeightPlot(0.5, "weight_plot_resample05.mp4", interval = 0.3)
