


toy.example.configuration <- smcToyExample()



# toy.example.configuration

# result <- Smc(toy.example.configuration, number.of.particles = 10, number.of.replicates = 5, verbose = T)

result <- Smc(toy.example.configuration, verbose = T) # toy.example.configuration, number.of.particles = 3, number.of.replicates = 5, verbose = T)

result$all.particles[[4]]

result$all.weights[[10]]

hsv(result$all.weights[[10]])

distance.max <- max(unlist(result$all.particles))
run.length <- length(result$effective.sample.sizes)
all.thetas <- unlist(result$all.thetas)
use.thetas <- c(min(all.thetas), max(all.thetas))

PlotParticles(result$all.particles[[10]], result$all.weights[[10]], cex = 1, epsilon = result$epsilons[[10]])

for(i in 1:length(result$all.particles)) {
  PlotParticles(result$all.particles[[i]], result$all.weights[[i]], 0.5, use.max = distance.max)
}


VisualiseToyExampleState(result, 10, use.max = distance.max, use.run.length = run.length, use.thetas = use.thetas)


CreateGifAnimation(result, "test_smc_movie2.gif", skip.frames = 1)

