postfix <- "_exp5"
fig.dir <- "exp5"

actual.structure.as.vector <- as.vector(actual.structure)


CalculatePostProbTheta <- function(myPhi, myVariance) {
  
  print(paste("myPhi: ", myPhi, "myVariance: ", myVariance))
  
  my.cov.mat <- matrix(
    sapply(1:grid.length, function(x1) {
      sapply(1:grid.length, function(y1) {
        sapply(1:grid.length, function(x2) {
          sapply(1:grid.length, function(y2) {
            # CalculateCovariance(x1, x2, y1, y2)
            myVariance * exp(-sqrt((x1 - x2) ^ 2 + (y1 - y2) ^ 2) / myPhi)
          })
        })
      })
    }), nrow = grid.length ^ 2, ncol = grid.length ^ 2, byrow = T
  )
  mu.x <- rep(kMean, grid.length^2)
  
  inv.actual.cov.mat <- solve(my.cov.mat)
  det.actual.cov.mat <- determinant(my.cov.mat)
  log.prob <- -(1 / 2) * c(det.actual.cov.mat$sign * det.actual.cov.mat$modulus) - (1 / 2) %*% t(actual.structure.as.vector - mu.x) %*% inv.actual.cov.mat %*% (actual.structure.as.vector - mu.x)
  
  exp(log.prob)
}


phi.points <- seq(5, 15, 0.5)
variance.points <- seq(2, 6, 0.5)


post.prob.eval.points <- sapply(phi.points, function(myPhi) {
  sapply(variance.points, function(myVariance) {
    CalculatePostProbTheta(myPhi, myVariance)
  })
})

post.prob.eval.points.matrix <-
  matrix(post.prob.eval.points, nrow = length(phi.points), ncol = length(variance.points), byrow =T)

png(paste("../../abcR_doc/fig/", fig.dir, "/likelihood_on_grid", postfix, ".png", sep = ""))
filled.contour(phi.points, variance.points, post.prob.eval.points.matrix)
# image(phi.points, variance.points, post.prob.eval.points.matrix)
title(latex2exp('Likelihood $p(\\theta | x)$'), xlab = latex2exp('$\\phi$'), ylab = latex2exp('$\\sigma^{2}$'))
dev.off()


filled.contour(phi.points, variance.points, post.prob.eval.points.matrix, 
               plot.axes = points(variogram.distance.ordered.parameter.matrix[ , 2], variogram.distance.ordered.parameter.matrix[ , 4], col = 'black', pch = 19, cex = 0.3))
title(latex2exp('Likelihood $p(\\theta | x)$'), xlab = latex2exp('$\\phi$'), ylab = latex2exp('$\\sigma^{2}$'))


png(paste("../../abcR_doc/fig/", fig.dir, "/distance_example_100_closest_", postfix, ".png", sep = ""))
filled.contour(phi.points, variance.points, post.prob.eval.points.matrix, 
               plot.axes = points(variogram.distance.ordered.parameter.matrix[1:100, 2], 
                                  variogram.distance.ordered.parameter.matrix[1:100, 4], col = 'black', pch = 19, cex = 0.3))
title(latex2exp('Likelihood $p(\\theta | x)$'), xlab = latex2exp('$\\phi$'), ylab = latex2exp('$\\sigma^{2}$'))
dev.off()


png(paste("../../abcR_doc/fig/", fig.dir, "/distance_example_1000_samples_", postfix, ".png", sep = ""))
filled.contour(phi.points, variance.points, post.prob.eval.points.matrix, 
               plot.axes = points(variogram.distance.ordered.parameter.matrix[, 2], 
                                  variogram.distance.ordered.parameter.matrix[, 4], col = 'black', pch = 19, cex = 0.3))
title(latex2exp('Likelihood $p(\\theta | x)$'), xlab = latex2exp('$\\phi$'), ylab = latex2exp('$\\sigma^{2}$'))
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/distance_plot_1000_samples_", postfix, ".png", sep = ""))
plot(variogram.distance.ordered.parameter.matrix[ , 1], type = "p", cex = 0.3, ann = F)
title(main = 'Sorted distance plot', xlab = 'Index', ylab = 'Distance')
dev.off()

# variogram.distance.ordered.parameter.matrix

points(abc.variogram.fit.estimates[ , 2], abc.variogram.fit.estimates[ , 1])
plot(abc.variogram.fit.estimates[ , 2], abc.variogram.fit.estimates[ , 1])





