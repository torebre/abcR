
my.toy.example.results <- ma.example.results

TempDistanceFunction <- function(x) { ma.configuration$DistanceFunction(unlist(x)) }
DistanceFunctionVectorized <- Vectorize(TempDistanceFunction)

animation::saveVideo(
  for(i in seq(1, run.length, by = 1)) {

    # i <- 1
    thetas <- unlist(ma.example.results[["all.thetas"]][i])
    thetas.weights <- unlist(ma.example.results[["all.weights"]][i])
    particles <- ma.example.results[["all.particles"]][[i]]
    epsilon <- unlist(ma.example.results[["epsilons"]][i])

    # op <- par("mfrow" = c(2, 2))

    theta.colors <- rep(NA, length(thetas.weights))
    for(j in 1:length(theta.colors)) {
      theta.colors[j] <- hsv(h = 0, s = 1 - thetas.weights[j], v = 1 - thetas.weights[j])
    }

    # TODO There are 2 theta values here

    # plot(y = thetas.weights, x = thetas, xlim = c(-3, 3), ylim = c(0, 1), pch = 16, cex = 0.5, col = colors, ann = F)
    # title(xlab = latex2exp("$\\theta$"))

    my.number.of.replicates <- 1 #length(particles[[1]])
    my.number.of.particles <- length(particles) * my.number.of.replicates
    colors <- rep(NA, my.number.of.particles)
    particle.weights <- rep(NA, my.number.of.particles)


    # Plot weights

    # for(j in 1:length(particles)) {
    #   for(k in 1:1) { #length(particles[[1]])) {
    #     current.particle.index <- (j - 1) * my.number.of.replicates + k
    #     colors[current.particle.index] <- hsv(h = 0, s = 1 - thetas.weights[j], v = 1 - thetas.weights[j])
    #     particle.weights[current.particle.index] <- thetas.weights[j]
    #   }
    # }
    # temp.particles <- DistanceFunctionVectorized(particles)
    # plot(y = particle.weights, x = temp.particles,
    #      xlim = c(0, 200), ylim = c(0, 1), pch = 16, cex = 0.5, col = colors, ann = F)
    # title(xlab = "x")
    # abline(v = -epsilon)
    # abline(v = epsilon)

      particle.vector <- ma.example.results[["all.particles"]][[i]]
      distances <- sapply(particle.vector, function(x) { DistanceFunctionRaw(unlist(x)) })
      plot(1:length(distances), distances, pch = 16, cex = 0.1, ylim = c(0, 1000))
      abline(h = ma.example.results[["epsilons"]][[i]])

    # abcProject::PlotParticles(particles, weights, cex = 0.3, particle.max, epsilon)

    # resampling.indices <- ResampleThetas(length(thetas.weights), thetas.weights)
    # PlotThetaHistogram(thetas[resampling.indices], c(-3, 3))
    #
    # abcProject::PlotEssTrace(my.toy.example.results, i, use.run.length = run.length)
    # par(op)
  }, video.name = "test3.mp4", ffmpeg = "avconv", interval = 0.1)
