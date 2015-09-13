library(geoR)

# my.mean = 10
# variance = 9
# 
# # par(mfrow = c(2, 2))
# 
# simulation = grf(50 * 50, grid = cbind(1:50, rep(1, 50)), cov.model = "exponential", 
#                  cov.pars = c(variance, 5), mean = rep(my.mean, 50));
# plot(simulation$data, type = "l", ann = FALSE)
# title(expression(paste("Exp. corr. func. with ", phi == 5)), cex.main = 1)


# Problem 3a
grid.length = 30
simulation = grf(n = grid.length^2, grid = expand.grid(1:grid.length, 1:grid.length), 
                 cov.model = "exponential", cov.pars = c(4, 10), mean = 10)

filled.contour(1:grid.length, 1:grid.length, 
               matrix(simulation$data, nrow = grid.length, ncol = grid.length), color = heat.colors)

# # Display an estimate of the marginal distribution at 
# # an arbitrary location
# truehist(simulation$data, ann = F)
# # Superimpose the theoretical marginal distribution
# testFunction = Vectorize(function(x) {dnorm(x, 10, 2)})
# curve(testFunction, add = T)
# 
# # Compute the empirical variogram using the 
# # full distribution, and plot it
# variogram.full = variog(simulation)
# plot(variogram.full, ylim = c(0, 8))
# lines.variomodel(cov.model = "exponential", cov.pars=c(4, 10), nugget = 0, col = "blue")
