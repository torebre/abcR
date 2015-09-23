# ABC approach

# Set up mean and covariance matrix for x
mu.x.prior <- matrix(rep(kMean, grid.length^2), ncol = 1)
cov.mat.x.prior <- matrix(sapply(1:grid.length, function(x1) {
  sapply(1:grid.length, function(y1) {
    sapply(1:grid.length, function(x2) {
      sapply(1:grid.length, function(y2) {
        CalculateCovariance(x1, x2, y1, y2)    
      })
    })
  })
}), nrow = grid.length^2, ncol = grid.length^2, byrow = T)
# image(cov.mat.x.prior)
# filled.contour(1:grid.length^2, 1:grid.length^2, cov.mat.x.prior)


# kTolerance <- 2
kTolerance <- 2
StatisticDistanceFunction <-
  function(proposed.sample.statistic, observed.statistic) {
    sqrt((proposed.sample.statistic - observed.statistic) ^ 2)
  }

number.of.abc.samples <- 50
abc.samples <- vector('list', number.of.abc.samples)
counter <- 1

while (counter <= length(abc.samples)) {
  abc.prior <- matrix(mvrnorm(mu = mu.x.prior, Sigma = cov.mat.x.prior), nrow = grid.length, ncol = grid.length)
  abc.prior.obs.points.mean <- mean(sapply(1:number.of.observations, function(obs.number) {
    abc.prior[y.coords[obs.number, 1], y.coords[obs.number, 2]]
  }))
  
  if (StatisticDistanceFunction(abc.prior.obs.points.mean, y.avg) < kTolerance) {
    print(paste("Got sample: ", counter))
    abc.samples[[counter]] <- abc.prior
    counter <- counter + 1
  }
}

abc.samples.mean <-
  sapply(1:grid.length ^ 2, function(x) {
    mean(sapply(abc.samples, function(abc.sample) {
      abc.sample[[x]]
    }))
  })

abc.samples.mean.matrix <- matrix(abc.samples.mean, nrow = grid.length, ncol = grid.length)
# filled.contour(1:grid.length, 1:grid.length, abc.samples.mean.matrix, color = kColours)
filled.contour(1:grid.length, 1:grid.length,
               abc.samples.mean.matrix, color = kColours, 
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title("Mean of samples from x given y average using ABC")


abc.samples.var.matrix <- matrix(sapply(1:grid.length^2, function(x) {
  var(sapply(abc.samples, function(abc.sample) {
    abc.sample[[x]]
  }))}), nrow = grid.length, ncol = grid.length, byrow = T)
filled.contour(1:grid.length, 
               1:grid.length, 
               abc.samples.var.matrix, color = kColours, 
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title('ABC: Variance x given y average')

