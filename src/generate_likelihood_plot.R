library(latex2exp)

postfix <- "_likelihood"
fig.dir <- ""

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


phi.points <- seq(5, 35, 1)
variance.points <- seq(2, 15, 1)


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
title(latex2exp('Likelihood $p(x|\\theta)$'), xlab = latex2exp('$\\phi$'), ylab = latex2exp('$\\sigma^{2}$'))
dev.off()
