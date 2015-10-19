



CalculatePostProbTheta <- function(myPhi, myVariance) {
  actual.cov.mat <- matrix(
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
  
  det.actual.cov.mat <- det(actual.cov.mat)
  inv.actual.cov.mat <- solve(actual.cov.mat)
  actual.structure.as.vector <- as.vector(actual.structure)
  
  return(-(1 / 2) * log(det.actual.cov.mat) - (1 / 2) %*% t(actual.structure.as.vector - mu.x) %*% inv.actual.cov.mat %*% (actual.structure.as.vector - mu.x))
}


phi.points <- seq(5, 15, 0.5)
variance.points <- seq(2, 6, 0.5)

post.prob.eval.points <- sapply(phi.points, function(myPhi) {
  sapply(variance.points, function(myVariance) {
    CalculatePostProbTheta(myPhi, myVariance)
  })
})

post.prob.eval.points.matrix <-
  matrix(
    post.prob.eval.points, nrow = length(phi.points), ncol = length(variance.points), byrow = T
  )

filled.contour(phi.points, variance.points, post.prob.eval.points.matrix)
