mu.x.given.y.matrix <- matrix(NA, nrow = grid.length, ncol = grid.length, byrow = T)
x.counter <- 1
y.counter <- 1
for(i in 1:grid.length) {
  for(j in 1:grid.length) {
    if(IsPointObservation(i, j)) {
      mu.x.given.y.matrix[i, j] <- NA # observations[y.counter]
      y.counter <- y.counter + 1
    }
    else {
      mu.x.given.y.matrix[i, j] <- mu.x.given.y[x.counter]
      x.counter <- x.counter + 1
    }
  }
}


filled.contour(1:grid.length, 1:grid.length, mu.x.given.y.matrix, color = kColours,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title(main = 'Expected values x given y')



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
