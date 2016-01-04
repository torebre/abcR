library(geoR)

SamplePriorPhi <- function() {
  # 10
  runif(n = 1, min = 5, max = 25)
}

SamplePriorVariance <- function() {
  # 4
  runif(n = 1, min = 2, max = 20)
}


GenerateParticlesSpatial <-
  function(my.number.of.particles, my.number.of.replicates) {
    # Setup the sample coordinates
    all.sample.points <- matrix(NA, nrow = grid.length ^ 2, ncol = 2)
    for (i in 1:grid.length) {
      for (j in 1:grid.length) {
        all.sample.points[(i - 1) * grid.length + j,] <- c(i, j)
      }
    }
    my.samples <- list()
    for (i in 1:my.number.of.particles) {
      prior.phi <- SamplePriorPhi()
      prior.variance <- SamplePriorVariance()
      my.replicates <-
        array(NA, c(my.number.of.replicates, grid.length, grid.length))
      
      # print(dim(my.replicates))
      
      for (j in 1:my.number.of.replicates) {
        my.replicates[j, , ] <- matrix(grf(grid.length ^ 2, all.sample.points, cov.model = "exp", cov.pars = c(prior.variance, prior.phi), mean = kMean)$data, grid.length, grid.length)
      }
      my.samples[[i]] <- my.replicates
      
    }
    return(my.samples)
  }