library(geoR)


actual.data.empirical.variogram <- variog(coord = y.coords, data = observations)


# Superimpose the true function


# TODO Can some initial values be provided here?
actual.data.empirical.variomodel <- variofit(actual.data.empirical.variogram, cov.model = "exponential")

actual.data.empirical.variomodel$cov.pars

png(paste("../../abcR_doc/fig/", fig.dir, "/variogram_example_and_true_function", postfix, ".png", sep = ""))
plot(actual.data.empirical.variogram)
lines.variomodel(cov.model = "exponential", cov.pars=c(kVariance, kPhi), nugget = 0, col = "blue")
lines.variomodel(cov.model = "exponential", cov.pars = actual.data.empirical.variomodel$cov.pars, nugget = 0, col = "red")
dev.off()

# abc.samples[[counter]] <- list(abc.prior = abc.prior, prior.phi = prior.phi, 
# prior.variance = prior.variance, prior.mean = prior.mean, prior.obs.noise = prior.obs.noise)

abc.variogram.fit.estimates = matrix(NA, nrow = length(abc.samples), ncol = 2)

for(i in 1:length(abc.samples)) {
  print(paste("Looking at sample ", i))
  my.empirical.variogram <- variog(coord = y.coords, data = abc.samples[[i]]$abc.prior[y.coords])
  my.empirical.variomodel <- variofit(my.empirical.variogram, ini.cov.pars = c(abc.samples[[i]]$prior.variance, abc.samples[[i]]$prior.phi), cov.model = "exponential")
  abc.variogram.fit.estimates[i, 1] <- my.empirical.variomodel$cov.pars[1]
  abc.variogram.fit.estimates[i, 2] <- my.empirical.variomodel$cov.pars[2]
}

# abc.sample.observations <- abc.samples[[1]]$abc.prior[y.coords]

plot(abc.variogram.fit.estimates[ , 1], abc.variogram.fit.estimates[ , 2])
abc.variogram.fit.estimates


GetDifferenceInVariogramEstimates <- function(my.phi, my.variance) {
  (abs(actual.data.empirical.variomodel$cov.pars[1] - my.variance))^2 + (abs(actual.data.empirical.variomodel$cov.pars[2] - my.phi))^2
  # (abs(4 - my.variance))^2 + (abs(10 - my.phi))^2
}

abc.distance.variogram.parameters <- matrix(NA, nrow = length(abc.samples), ncol = 5)
for(i in 1:length(abc.samples)) {
  distance <- GetDifferenceInVariogramEstimates(abc.variogram.fit.estimates[i, 2], abc.variogram.fit.estimates[i, 1])
  # distance <- GetDifferenceInVariogramEstimates(abc.samples[[i]]$prior.phi, abc.samples[[i]]$prior.variance)
  abc.distance.variogram.parameters[i , ] = c(distance, abc.samples[[i]]$prior.phi, abc.samples[[i]]$prior.mean, abc.samples[[i]]$prior.variance, abc.samples[[i]]$prior.obs.noise)
}



indices.ordered.by.variogram.distance <- order(abc.distance.variogram.parameters[, 1])

variogram.distance.ordered.parameter.matrix <- abc.distance.variogram.parameters[indices.ordered.by.variogram.distance , ]

plot(variogram.distance.ordered.parameter.matrix[ , 1], type = "p", cex = 0.3)


plot(variogram.distance.ordered.parameter.matrix[ , 2], variogram.distance.ordered.parameter.matrix[ , 4], pch = 16, cex = 0.5)

plot(variogram.distance.ordered.parameter.matrix[1:100 , 2], variogram.distance.ordered.parameter.matrix[1:100 , 4], pch = 16, cex = 0.5)

plot(variogram.distance.ordered.parameter.matrix[1:100 , 2], variogram.distance.ordered.parameter.matrix[1:100 , 1], pch = 16, cex = 0.5)
plot(variogram.distance.ordered.parameter.matrix[1:100, 4], variogram.distance.ordered.parameter.matrix[1:100 , 1], pch = 16, cex = 0.5)

plot(variogram.distance.ordered.parameter.matrix[ , 2])
plot(variogram.distance.ordered.parameter.matrix[ , 4])

plot(variogram.distance.ordered.parameter.matrix[1:100, 2], variogram.distance.ordered.parameter.matrix[1:100, 4])

plot(abc.variogram.fit.estimates[ , 1])
plot(abc.variogram.fit.estimates[ , 2])

truehist(abc.variogram.fit.estimates[ , 1])
truehist(abc.variogram.fit.estimates[ , 2])

