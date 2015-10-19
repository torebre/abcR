library(geoR)

empirical.variogram <- variog(coord = y.coords, data = observations)
plot(empirical.variogram)

lines.variomodel(cov.model = "exponential", cov.pars=c(kVariance, kPhi), nugget = 0, col = "blue")

# TODO Can some initial values be provided here?
empirical.variomodel <- variofit(empirical.variogram, cov.model = "exponential")

empirical.variomodel$cov.pars

