library(latex2exp)
library(MASS)


GenerateWeightPlotAtEnd <- function(resample.ratio, picture.file.name) {
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

  resample.limit <- 50 * resample.ratio
  run.length <- length(toy.example.results[["effective.sample.sizes"]])
  particle.max <- max(abs(unlist(toy.example.results[["all.particles"]])))

  index.to.use <- run.length - 1

      thetas <- unlist(toy.example.results[["all.thetas"]][index.to.use])
      thetas.weights <- unlist(toy.example.results[["all.weights"]][index.to.use])
      particles <- toy.example.results[["all.particles"]][[index.to.use]]
      epsilon <- unlist(toy.example.results[["epsilons"]][index.to.use])

      png(picture.file.name, width = 720, height = 480)
      par("mfrow" = c(1, 3))

      theta.colors <- rep(NA, length(thetas.weights))
      for(j in 1:length(theta.colors)) {
        theta.colors[j] <- hsv(h = 0, s = 1 - thetas.weights[j], v = 1 - thetas.weights[j])
      }

      plot(y = thetas.weights, x = thetas, xlim = c(-3, 3), ylim = c(0, 1), pch = 16, cex = 1, col = theta.colors, ann = F)
      title(xlab = latex2exp("$\\theta$"))

      # my.number.of.replicates <- length(particles[[1]])
      # my.number.of.particles <- length(particles) * my.number.of.replicates
      # colors <- rep(NA, my.number.of.particles)
      # particle.weights <- rep(NA, my.number.of.particles)
      #
      # for(j in 1:length(particles)) {
      #   for(k in 1:length(particles[[1]])) {
      #     current.particle.index <- (j - 1) * my.number.of.replicates + k
      #     colors[current.particle.index] <- hsv(h = 0, s = 1 - thetas.weights[j], v = 1 - thetas.weights[j])
      #     particle.weights[current.particle.index] <- thetas.weights[j]
      #   }
      # }
      # plot(y = particle.weights, x = unlist(particles),
      #      xlim = c(-particle.max, particle.max), ylim = c(0, 1), pch = 16, cex = 0.5, col = colors, ann = F)
      # title(xlab = "x")
      # abline(v = -epsilon)
      # abline(v = epsilon)

      # abcProject::PlotParticles(particles, weights, cex = 0.3, particle.max, epsilon)

      resampling.indices <- ResampleThetas(length(thetas.weights), thetas.weights)
      PlotThetaHistogram(thetas[resampling.indices], c(-3, 3))

      abcProject::PlotEssTrace(toy.example.results, run.length, resample.limit = resample.limit)

      dev.off()
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

GenerateWeightPlotAtEnd(0, "weight_plot_resample0.png")
GenerateWeightPlotAtEnd(0.5, "weight_plot_resample05.png")


