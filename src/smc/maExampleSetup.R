
maConfiguration <- smcMovingAverageExample(create.debug.variables = T)


SampleFunction <- maConfiguration[["SampleFunction"]]
ForwardKernelSample <- maConfiguration[["ForwardKernelSample"]]
DistanceFunction <- maConfiguration[["DistanceFunction"]]
GenerateRandomPrior <- maConfiguration[["GenerateRandomPrior"]]


GenerateRandomPrior()


SampleFunction()
