library(MASS)
library(geoR)

test.length <- 100

my.sample.coords <- matrix(NA, test.length^2, 2)
counter <- 1
for(i in 1:test.length) {
  for(j in 1:test.length) {
    current.row <- counter
    counter <- counter + 1
    my.sample.coords[current.row, 1] <- i
    my.sample.coords[current.row, 2] <- j
  }
}


test.cov.mat <- matrix(NA, nrow = grid.length^2, ncol = grid.length^2)



test.cov.mat <- matrix(sapply(1:test.length, function(x1) {
  sapply(1:test.length, function(y1) {
    sapply(1:test.length, function(x2) {
      sapply(1:test.length, function(y2) {
        CalculateCovariance(x1, x2, y1, y2)    
      })
    })
  })
}), nrow = test.length^2, ncol = test.length^2, byrow = T)


start <- proc.time()
mu.vector <- rep(kMean, test.length^2)
test <- matrix(mvrnorm(mu = mu.vector, Sigma = actual.cov.mat), nrow = test.length, ncol = test.length)
print(proc.time()- start)


# my.sample.coords <- matrix(NA, my.sample.dims[1] * my.sample.dims[2], 2)
# my.sample.all.observations <- rep(NA, my.sample.dims[1] * my.sample.dims[2])

start <- proc.time()
test2 <- grf(test.length^2, my.sample.coords, cov.pars = c(kVariance, kPhi))
print(proc.time() - start)

test.matrix <- matrix(test2$data, nrow = test.length, ncol = test.length)
