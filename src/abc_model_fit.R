

df <- data.frame(abc.distance.parameters)
fit <- lm(X1 ~ X2 + X3 + X4 + X5, data = df)
summary(fit)


# abc.samples[[counter]] <- list(abc.prior = abc.prior, prior.phi = prior.phi, prior.variance = prior.variance, prior.mean = prior.mean, prior.obs.noise = prior.obs.noise)


length(abc.samples)

abc.distance.parameters

indices.ordered.by.distance <- order(abc.distance.parameters[, 1])

ordered.parameter.matrix <- abc.distance.parameters[indices.ordered.by.distance, ]

plot(ordered.parameter.matrix[ , 1], type = "p", cex = 0.3)


points(ordered.parameter.matrix[ , 2], ordered.parameter.matrix[ , 4])

plot(ordered.parameter.matrix[ , 2], ordered.parameter.matrix[ , 4], pch = 16, cex = 0.5)
plot(ordered.parameter.matrix[1:20 , 2], ordered.parameter.matrix[1:20 , 4], pch = 16, cex = 0.5)
