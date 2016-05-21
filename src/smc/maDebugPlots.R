

temp.function <- Vectorize(function(x) {FindNextEpsilon2(x, current.epsilon, particles, weights, 0.9, DistanceFunction)})
curve(temp.function, from = 0, to = current.epsilon, ann = F) # ylim = c(-10, 100))
title(xlab = latex2exp("$\\epsilon_{candidate}$"))
abline(v = new.epslion)


current.epsilon
FindNextEpsilon2(current.epsilon, current.epsilon, particles, weights, 0.9, DistanceFunction)


weight.updates2 <-CalculateWeightUpdates2(particles, current.epsilon, current.epsilon, DistanceFunction)

weights.new2 <- NormalizeVector2(weights * weight.updates2)

debug.variables2 <- ma.configuration[["GetDebugVariables"]]()



calc.weight.updates <- CalculateWeightUpdates2(particles, current.epsilon, current.epsilon, DistanceFunction)
calc.weight.updates2 <- CalculateWeightUpdates2(particles, current.epsilon, current.epsilon - 0.001, DistanceFunction)

filtered.distances <- distances[distances < current.epsilon]
hist(filtered.distances - current.epsilon, breaks = 100, ann = F)
title(xlab = latex2exp("$d(x,y) - \\epsilon_{current}$"), ylab = "Frequency")


