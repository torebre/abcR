postfix <- "_exp3"
fig.dir <- "exp3"


png(paste("../../abcR_doc/fig/", fig.dir, "/cov_var_func2", postfix, ".png", sep = ""))
curve(CovarianceFunction, from = 0, to = 100, n = 100, col = 'red', ann = FALSE)
title("Exponential cov. func.")
legend("topright", legend = parse(text = paste("phi", "==", kPhi)), lty = c(1, 1), lwd = c(2.5, 2.5), col = 'red')
dev.off()


png(paste("../../abcR_doc/fig/", fig.dir, "/actual_structure_no_observations", postfix, ".png", sep = ""))
filled.contour(1:grid.length, 1:grid.length,
               actual.structure, color = kColours)
title(main = 'Actual structure')
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/actual_structure_with_observations", postfix, ".png", sep = ""))
filled.contour(1:grid.length, 1:grid.length,
               actual.structure, color = kColours,
              plot.axes = points(y.coords[ , 1], y.coords[ , 2], col = 'black', pch = 19, cex = 0.3))
title(main = 'Actual structure')
title(sub = 'Observations superimposed as black dots')
dev.off()


png(paste("../../abcR_doc/fig/", fig.dir, "/cov_mat_x", postfix, ".png", sep = ""))
filled.contour(1:grid.length^2, 1:grid.length^2, cov.mat.x, color = kColours)
title("Covariance matrix x")
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/cov_mat_y", postfix, ".png", sep = ""))
filled.contour(1:dim(y.coords)[1], 1:dim(y.coords)[1], cov.mat.y, color = kColours)
title('Covariance matrix y')
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/cov_mat_x_y", postfix, ".png", sep = ""))
filled.contour(1:dim(y.coords)[1], 1:grid.length^2, cov.mat.y.x)
title('Covariance between x and y')
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/cov_x_given_y", postfix, ".png", sep = ""))
filled.contour(1:grid.length^2, 1:grid.length^2, cov.mat.x.given.y)
title('Cov x given y')
dev.off()


png(paste("../../abcR_doc/fig/", fig.dir, "/exp_x_given_y", postfix, ".png", sep = ""))
filled.contour(1:grid.length, 1:grid.length, matrix(mu.x.given.y, nrow = grid.length, ncol = grid.length, byrow = T), color = kColours)
title('Expected value x given y')
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/var_x_given_y", postfix, ".png", sep = ""))
filled.contour(1:grid.length, 1:grid.length, var.x.given.y.matrix, color = kColours)
title('Variance x given y')
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/exp_x_given_y_avg", postfix, ".png", sep = ""))
filled.contour(1:grid.length, 1:grid.length, matrix(mu.x.given.y.avg, nrow = grid.length, ncol = grid.length, byrow = T), color = kColours,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19, cex = 0.3))
title(main = 'Expected value x given y avg')
dev.off()


png(paste("../../abcR_doc/fig/", fig.dir, "/cov_x_given_y_avg", postfix, ".png", sep = ""))
filled.contour(1:grid.length^2, 1:grid.length^2, cov.mat.x.given.y.avg, color = kColours)
title('Cov x given y avg')
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/var_x_given_y_avg", postfix, ".png", sep = ""))
filled.contour(1:grid.length, 1:grid.length, var.x.given.y.avg.matrix, color = kColours,
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19, cex = 0.3))
title(main = 'Variance x given y avg')
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/abc_samples_mean", postfix, ".png", sep = ""))
filled.contour(1:grid.length, 1:grid.length,
               abc.samples.mean.matrix, color = kColours, 
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
text = parse(text = paste("hat(x,bar(y))"))
title(latex2exp('ABC samples $\\hat{E(x|\\bar{y})}$'))
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/abc_var_x_given_y_avg", postfix, ".png", sep = ""))
filled.contour(1:grid.length, 
               1:grid.length, 
               abc.samples.var.matrix, color = kColours, 
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title('ABC: Variance x given y average')
dev.off()

png(paste("../../abcR_doc/fig/", fig.dir, "/abc_mean_extra_samples", postfix, ".png", sep = ""))
filled.contour(1:grid.length, 1:grid.length,
               filtered.samples.mean.matrix, color = kColours, 
               plot.axes = points(y.coords[ , 1], y.coords[ , 2], pch = 19))
title(paste("ABC. Threshold: ", kVariableThreshold, "Samples: ", number.of.filtered.samples))
dev.off()

