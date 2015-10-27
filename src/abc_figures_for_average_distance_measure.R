
# abc.distance.parameters.avg
indices.ordered.by.avg.distance <- order(abc.distance.parameters.avg[, 1])
avg.distance.ordered.parameter.matrix <- abc.distance.parameters.avg[indices.ordered.by.avg.distance , ]


filled.contour(phi.points, variance.points, post.prob.eval.points.matrix, 
               plot.axes = points(variogram.distance.ordered.parameter.matrix[ , 2], variogram.distance.ordered.parameter.matrix[ , 4], col = 'black', pch = 19, cex = 0.3))
title(latex2exp('Likelihood $p(\\theta | x)$'), xlab = latex2exp('$\\phi$'), ylab = latex2exp('$\\sigma^{2}$'))


png(paste("../../abcR_doc/fig/", fig.dir, "/avg_distance_measure_example_100_closest_", postfix, ".png", sep = ""))
filled.contour(phi.points, variance.points, post.prob.eval.points.matrix, 
               plot.axes = points(avg.distance.ordered.parameter.matrix[1:100, 2], 
                                  avg.distance.ordered.parameter.matrix[1:100, 4], col = 'black', pch = 19, cex = 0.3))
title(latex2exp('Likelihood $p(\\theta | x)$'), xlab = latex2exp('$\\phi$'), ylab = latex2exp('$\\sigma^{2}$'))
dev.off()


png(paste("../../abcR_doc/fig/", fig.dir, "/distance_example_1000_samples_", postfix, ".png", sep = ""))
filled.contour(phi.points, variance.points, post.prob.eval.points.matrix, 
               plot.axes = points(variogram.distance.ordered.parameter.matrix[, 2], 
                                  variogram.distance.ordered.parameter.matrix[, 4], col = 'black', pch = 19, cex = 0.3))
title(latex2exp('Likelihood $p(\\theta | x)$'), xlab = latex2exp('$\\phi$'), ylab = latex2exp('$\\sigma^{2}$'))
dev.off()

# variogram.distance.ordered.parameter.matrix

points(abc.variogram.fit.estimates[ , 2], abc.variogram.fit.estimates[ , 1])
plot(abc.variogram.fit.estimates[ , 2], abc.variogram.fit.estimates[ , 1])





