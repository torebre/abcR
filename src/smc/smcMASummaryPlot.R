
op <- par(no.readonly = TRUE)
# op <- par("mfrow" = c(2, 2))
layout(matrix(c(0,1,1,0,2,2,3,3), 2, 4, byrow = TRUE), respect = F)
thetas.at.step <- thetas[[run.length]]

plot.new()
my.filled.contour(theta1.range, theta2.range, likelihood.map,
                  plot.axes = {points(t(thetas.at.step), pch = 19, cex = 0.1); axis(1); axis(2)}, xlim = c(-2, 2), ylim = c(-1, 1))
title(xlab = latex2exp("$\\theta_{1}$"), ylab = latex2exp("$\\theta_{2}$"))

plot.new()
my.filled.contour(theta1.range, theta2.range, likelihood.map,
                  plot.axes = {axis(1); axis(2)}, xlim = c(-2, 2), ylim = c(-1, 1))
title(xlab = latex2exp("$\\theta_{1}$"), ylab = latex2exp("$\\theta_{2}$"))

temp <- unlist(thetas.at.step)
k <- kde2d(temp[1, ], temp[2, ], n = 200, lims = c(c(-2, 2), c(-1, 1)))
image(k, col = cm.colors(20))
title(xlab = latex2exp("$\\theta_{1}$"), ylab = latex2exp("$\\theta_{2}$"))

layout(matrix(c(1,2), 1, 2, byrow = TRUE), respect = F)
PlotEpsilonTrace(ma.example.results, i, use.run.length = run.length, use.eps.max = 500)
PlotEssTrace(ma.example.results, i, use.run.length = run.length)

par(op)


