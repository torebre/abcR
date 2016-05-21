#particles <- ma.example.results[["all.particles"]][[state.to.visualise]]


DistanceFunctionRaw <- ma.configuration[["DistanceFunction"]]

run.length <- length(ma.example.results[["all.particles"]])

ma.example.results[["effective.sample.sizes"]][900]
ma.example.results[["all.weights"]][900]

particle.vector <- ma.example.results[["all.particles"]][[900]]

# distances <- sapply(particle.vector, function(x) { DistanceFunctionRaw(unlist(x)) })
distances <- sapply(variable.env$particles, function(x) { DistanceFunctionRaw(unlist(x)) })
plot(1:length(distances), distances, pch = 16, cex = 0.1)


abline(h = ma.example.results[["epsilons"]][[900]], col = "red")
abline(h = current.epsilon, col = "red")

current.epsilon <- ma.example.results[["epsilons"]][[900]]


animation::saveGIF(
  for(i in seq(1, run.length, by = 10)) {
    particle.vector <- ma.example.results[["all.particles"]][[i]]
    distances <- sapply(particle.vector, function(x) { DistanceFunctionRaw(unlist(x)) })
    plot(1:length(distances), distances, pch = 16, cex = 0.1, ylim = c(0, 1000))
    abline(h = ma.example.results[["epsilons"]][[i]])
  }, movie.name = "/home/student/raw_distances_2.gif", interval = 0.3)

