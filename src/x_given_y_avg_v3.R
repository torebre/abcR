mu.x <- matrix(rep(kMean, grid.length^2), ncol = 1)
cov.mat.x <- actual.cov.mat

# x given y
mu.y <- matrix(rep(kMean, dim(y.coords)[1]), ncol = 1)
cov.mat.y.given.x <- kObsNoiseVar * diag(dim(y.coords)[1])
C2 <- solve(cov.mat.y.given.x + D %*% cov.mat.x %*% t(D))
mu.x.given.y <- mu.x + cov.mat.x %*% t(D) %*% C2 %*% matrix(observations - mu.y, nrow = length(mu.y), ncol = 1)
cov.mat.x.given.y <- cov.mat.x - cov.mat.x %*% t(D) %*% C2 %*% D %*% cov.mat.x
cov.mat.y <- cov.mat.y.given.x + D %*% cov.mat.x %*% t(D)
cov.mat.y.x <- D %*% cov.mat.x
var.x.given.y.matrix <- matrix(diag(cov.mat.x.given.y), nrow = grid.length, ncol = grid.length, byrow = T)


# x given y average
A <- matrix(1/dim(y.coords)[1], nrow = 1, ncol = dim(y.coords)[1])
mu.y.avg <- kMean
C <- solve(kObsNoiseVar * A %*% t(A) + A %*% D %*% cov.mat.x %*% t(D) %*% t(A))
mu.x.given.y.avg <- mu.x + cov.mat.x %*% t(D) %*% t(A) %*% C %*% (matrix(y.avg - mu.y.avg, nrow = 1, ncol = 1))
cov.mat.x.given.y.avg <- cov.mat.x - cov.mat.x %*% t(D) %*% t(A) %*% C %*% A %*% D %*% cov.mat.x
mu.x.given.y.avg.matrix <- matrix(cov.mat.x.given.y.avg, nrow = grid.length, ncol = grid.length, byrow = T)
var.x.given.y.avg.matrix <- matrix(diag(cov.mat.x.given.y.avg), nrow = grid.length, ncol = grid.length, byrow = T)
