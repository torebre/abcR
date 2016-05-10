number.of.particles <- 10
number.of.replicates <- 2
maConfiguration <- smcMovingAverageExample(create.debug.variables = T)


SampleFunction <- maConfiguration[["SampleFunction"]]
ForwardKernelSample <- maConfiguration[["ForwardKernelSample"]]
DistanceFunction <- maConfiguration[["DistanceFunction"]]
GenerateRandomPrior <- maConfiguration[["GenerateRandomPrior"]]


thetas <- GenerateRandomPrior(number.of.particles)
series.sample <- SampleFunction(thetas, number.of.replicates)
weights <- rep(1/number.of.particles, number.of.particles)

# plot(series.sample[[1]][1, ], type = "l")
# plot(series.sample[[1]][2, ], type = "l")
# dim(series.sample[[1]])


# maExample[["ForwardKernelSample"]] <- function(samples.old,
#                                                theta.old,
#                                                my.current.epsilon,
#                                                my.weights)

test1 <- ForwardKernelSample(series.sample, thetas, 3, weights)

