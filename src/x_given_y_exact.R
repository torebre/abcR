result <- matrix(NA, nrow = grid.length, ncol = grid.length, byrow = T)
x.counter <- 1
y.counter <- 1
for(i in 1:grid.length) {
  for(j in 1:grid.length) {
    if(IsPointObservation(i, j)) {
      result[i, j] <- observations[y.counter]
      y.counter <- y.counter + 1
    }
    else {
      result[i, j] <- mu.x.given.y[x.counter]
      x.counter <- x.counter + 1
    }
  }
}


# levels <- seq(5, 15, 0.5)
filled.contour(1:grid.length, 1:grid.length, result, color = kColours,
               # levels = levels,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title(main = 'Expected value x given y')



var.x.given.y.matrix <- matrix(NA, nrow = grid.length, ncol = grid.length, byrow = T)
x.counter <- 1
for(i in 1:grid.length) {
  for(j in 1:grid.length) {
    if(IsPointObservation(i, j)) {
      var.x.given.y.matrix[i, j] <- NA
    }
    else {
      var.x.given.y.matrix[i, j] <- cov.mat.x.given.y[x.counter, x.counter]
      x.counter <- x.counter + 1
    }
  }
}


filled.contour(1:grid.length, 1:grid.length, var.x.given.y.matrix, color = kColours,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title(main = 'Variance x given y')
