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

filled.contour(1:grid.length, 1:grid.length, result, color = kColours,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))

