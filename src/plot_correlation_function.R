# library(geoR)
library(rgl)



MyFunction <- function(distance) {
  2 * exp(-distance / 8)
}

MyCorrelationFunction <- function(myPhi, myVariance) {
  
  
}

persp3d(topo.mar$x, topo.mar$y, topo.lo$fit, color=colorlut[zcol],
        back="lines", xlim = c(0, max.coord), ylim = c(0, max.coord),
        xlab = "x", ylab = "y", zlab = "z")
grid3d(c("x", "y", "z"), n = 13)


curve(MyFunction, from = 0, to = 100, n = 100, col = 'red', ann = FALSE)
title("Exponential cov. func.")
legend("topright", legend = parse(text = paste("phi", "==", 8)), lty = c(1, 1), lwd = c(2.5, 2.5), col = 'red')


x <- seq(5, 15, 0.5)
y <- seq(2, 6, 0.5)
z <- outer(x^2,y^2,`+`)

z <- sapply(phi.points, function(myPhi) {
  sapply(variance.points, function(myVariance) {
    CalculatePostProbTheta(myPhi, myVariance)
  })
})

persp(x,y,z, col='blue')


