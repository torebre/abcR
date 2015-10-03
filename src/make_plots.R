


png("../../abcR_doc/fig/cov_var_func.png")
curve(CovarianceFunction, from = 0, to = 100, n = 100, col = 'red', ann = FALSE)
title("Exponential cov. func.")
text = as.vector(sapply(as.array(sequence),
                        function(x) parse(text = paste("phi", "==", x))))
legend("topright", legend = parse(text = paste("phi", "==", kPhi)), lty = c(1, 1), lwd = c(2.5, 2.5), col = 'red')
dev.off()


png("../../abcR_doc/fig/actual_structure.png")
filled.contour(1:grid.length, 1:grid.length,
               actual.structure, color = kColours, 
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], col = 'black', pch = 19, cex = 0.3))
title(main = 'Actual structure')
title(sub = '100 random observations superimposed as black dots')
dev.off()

png("../../abcR_doc/fig/actual_cov_mat.png")
filled.contour(actual.cov.mat)
dev.off()




filled.contour(1:x.number.of.points, 1:x.number.of.points, cov.mat.x)


filled.contour(1:grid.length, 1:grid.length, result, color = kColours,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title(main = 'x given y avg')


image(cov.mat.y)
title('Covariance matrix y')
filled.contour(cov.mat.y)
title('Covariance matrix y')

image(1:x.number.of.points, 1:number.of.observations, cov.mat.y.x)
title('Covariance between x and y')
filled.contour(cov.mat.y.x)
title('Covariance between x and y')

image(1:grid.length, 1:grid.length, mu.x.given.y.avg.matrix)
title(main = 'Expected value x given y avg')

filled.contour(1:grid.length, 1:grid.length, mu.x.given.y.avg.matrix, color = kColours,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title(main = 'Expected value x given y avg')





image(cov.mat.x.given.y.avg)
title(main = 'Cov matrix x given y avg')


filled.contour(1:x.number.of.points, 1:x.number.of.points, cov.mat.x.given.y.avg)
title('Cov x given y avg')


# image(1:grid.length, 1:grid.length, var.result, color = kColours)

filled.contour(1:grid.length, 1:grid.length, var.x.given.y.avg.matrix, color = kColours,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title(main = 'Variance x given y avg')







