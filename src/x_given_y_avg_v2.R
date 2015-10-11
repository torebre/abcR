# Set up mean and covariance matrix for x
print('Setting up mu.x')
mu.x <- matrix(rep(kMean, x.number.of.points), ncol = 1)

cov.mat.x <- matrix(sapply(1:x.number.of.points, function(i) {
  sapply(1:x.number.of.points, function(j) {
    CalculateCovariance(x.coords[i, 1], x.coords[j, 1], x.coords[i, 2], x.coords[j, 2])
  })
}), nrow = x.number.of.points, ncol = x.number.of.points, byrow = T)

A <- matrix(1/number.of.observations, nrow = 1, ncol = number.of.observations)





# Set up mean and covariance matrix for y
print('Setting up mu.y')
mu.y <- matrix(rep(kMean, number.of.observations), ncol = 1)

print('Setting up cov.mat.y')
cov.mat.y <- matrix(sapply(1:number.of.observations, function(x.index) {
  sapply(1:number.of.observations, function(y.index) {
    # CalculateCovariance(y.coords[x.index, 1], y.coords[y.index, 1],  y.coords[x.index, 2],  y.coords[y.index, 2])
    if(x.index == y.index) {
      kObsNoiseVar
    }
    else {
      0
    }
  })
}), nrow = number.of.observations, byrow = T)

print('Setting up cov.mat.y.x')
cov.mat.y.x <- t(sapply(1:number.of.observations, function(obs.index) {
  sapply(1:x.number.of.points, function(pred.index) {
    CalculateCovariance(y.coords[obs.index, 1], x.coords[pred.index, 1], y.coords[obs.index, 2], x.coords[pred.index, 2])
  })
}))


# Compute inverses
print('Computing inverses')
cov.mat.x.inv <- solve(cov.mat.x)
cov.mat.y.inv <- solve(cov.mat.y)

# Expressions for x given y
print('Expressions for x given y')
mu.x.given.y <- mu.x + t(cov.mat.y.x) %*% cov.mat.y.inv %*% (observations - mu.y)
cov.mat.x.given.y <- cov.mat.x - t(cov.mat.y.x) %*% cov.mat.y.inv %*% cov.mat.y.x

cov.mat.y.given.x <- cov.mat.y - cov.mat.y.x %*% cov.mat.x.inv %*% t(cov.mat.y.x)

cov.mat.y.avg.given.x <- A %*% cov.mat.y.given.x %*% t(A)

B1 <- A %*% cov.mat.y.x %*% cov.mat.x.inv
B2 <- A %*% (cov.mat.y - cov.mat.y.x %*% cov.mat.x.inv %*% t(cov.mat.y.x)) %*% t(A)

cov.mat.y.avg <- B2 + B1 %*% cov.mat.x %*% t(B1)
cov.mat.x.y.avg <- t(B1 %*% cov.mat.x)

cov.mat.y.avg.inv <- solve(cov.mat.y.avg)

mu.x.given.y.avg <- mu.x + cov.mat.x.y.avg %*% cov.mat.y.avg.inv %*% (matrix(y.avg, nrow = 1, ncol = 1) - mu.y.avg)
cov.mat.x.given.y.avg <- cov.mat.x - cov.mat.x.y.avg %*% cov.mat.y.avg.inv %*% t(cov.mat.x.y.avg)


# cov.mat.x.y.avg <- A %*% cov.mat.y.x %*% t(A)
# mu.y.avg <- A %*% mu.y
# cov.mat.y.avg <- A %*% cov.mat.y %*% t(A)
# 
# cov.mat.y.avg.inv <- matrix(1 / cov.mat.y.avg, nrow = 1, ncol = 1)
# mu.x.given.y.avg <- mu.x + cov.mat.x.y.avg %*% cov.mat.y.avg.inv %*% (y.avg - mu.y.avg)
# cov.mat.x.given.y.avg <- cov.mat.x - cov.mat.x.y.avg %*% cov.mat.y.avg.inv %*% t(cov.mat.x.y.avg)

print('Setting up result matrix')
result <- matrix(NA, nrow = grid.length, ncol = grid.length, byrow = T)
x.counter <- 1
y.counter <- 1
for(i in 1:grid.length) {
  for(j in 1:grid.length) {
    if(IsPointObservation(i, j)) {
      result[i, j] <- y.coords[y.counter]
      y.counter <- y.counter + 1
    }
    else {
      result[i, j] <- mu.x.given.y.avg[x.counter]
      x.counter <- x.counter + 1
    }
  }
}

# cov.mat.y.avg <- A %*% cov.mat.y %*% t(A)
# cov.mat.x.y.avg <- t(A %*% t(cov.mat.y.x) %*% cov.mat.x.inv) %*% (A %*% cov.mat.y %*% t(A))
# B1 <- A %*% t(cov.mat.y.x) %*% cov.mat.x.inv
# 
# B2 <- A %*% (cov.mat.y - t(cov.mat.y.x) %*% cov.mat.x.inv %*% cov.mat.y.x) %*% t(A)
# C <- B2 + B1 %*% cov.mat.x %*% t(B1)
# C.inv <- solve(C)
# mu.x.given.y.avg <- mu.x + t(B1 %*% cov.mat.x) %*% C.inv %*% (y.avg - mu.y.avg)


mu.x.given.y.avg.matrix <- matrix(NA, nrow = grid.length, ncol = grid.length, byrow = T)
x.counter <- 1
y.counter <- 1
for(i in 1:grid.length) {
  for(j in 1:grid.length) {
    if(IsPointObservation(i, j)) {
      mu.x.given.y.avg.matrix[i, j] <- NA # y.coords[y.counter]
      y.counter <- y.counter + 1
    }
    else {
      mu.x.given.y.avg.matrix[i, j] <- mu.x.given.y.avg[x.counter]
      x.counter <- x.counter + 1
    }
  }
}


# Set up the covariance matrix
# cov.mat.x.given.y.avg <- cov.mat.x - t(B1 %*% cov.mat.x) %*% C.inv %*% B1 %*% cov.mat.x

var.x.given.y.avg.matrix <- matrix(NA, nrow = grid.length, ncol = grid.length, byrow = T)
x.counter <- 1
y.counter <- 1
for(i in 1:grid.length) {
  for(j in 1:grid.length) {
    if(IsPointObservation(i, j)) {
      var.x.given.y.avg.matrix[i, j] <- 0
      y.counter <- y.counter + 1
    }
    else {
      # The entries on the diagonal are the variances
      var.x.given.y.avg.matrix[i, j] <- cov.mat.x.given.y.avg[x.counter, x.counter]
      x.counter <- x.counter + 1
    }
  }
}
