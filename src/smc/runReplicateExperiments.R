
toy.example.configuration <- smcToyExample()
toy.example.results <-
  Smc(
    toy.example.configuration,
    max.iterations = 100000,
    alpha = 0.9,
    number.of.particles = 3,
    number.of.replicates = 3,
    stop.epsilon = 0.01,
    verbose = T
  )

state.to.visualise <- 1 # length(toy.example.results[["effective.sample.sizes"]])

effective.sample.size <-
  toy.example.results[["effective.sample.sizes"]][state.to.visualise]
epsilon <- unlist(toy.example.results[["epsilons"]][state.to.visualise])
thetas <- unlist(toy.example.results[["all.thetas"]][state.to.visualise])
weights <- unlist(toy.example.results[["all.weights"]][state.to.visualise])
particles <- toy.example.results[["all.particles"]][[state.to.visualise]]

use.thetas <- c(-3, 3)

my.number.of.replicates <- length(particles[[1]])
my.number.of.particles <- length(particles) * my.number.of.replicates

sector.size <- 2 * pi / my.number.of.particles
particle.max <- max(abs(unlist(particles)))

cumulative.angle <- 0
x.coords <- rep(NA, my.number.of.particles)
y.coords <- rep(NA, my.number.of.particles)
colors <- rep(NA, my.number.of.particles)

for(j in 1:length(particles)) {
  for(k in 1:length(particles[[1]])) {
    particle.distance <- abs(particles[[j]][k])
    current.particle.index <- (j - 1) * my.number.of.replicates + k
    x.coords[current.particle.index] <- cos(cumulative.angle) * particle.distance
    y.coords[current.particle.index] <- sin(cumulative.angle) * particle.distance
    cumulative.angle <- cumulative.angle + sector.size
    colors[current.particle.index] <- hsv(h = weights[j])
  }
}

plot(
  x.coords,
  y.coords,
  pch = 16,
  cex = 0.5,
  xlim = c(-particle.max, particle.max),
  ylim = c(-particle.max, particle.max),
  col = colors,
  ann = F
)

plotrix::draw.circle(0, 0, epsilon)

title("d(x, 0)")

cumulative.sector <- 0
while(cumulative.sector < 2 * pi) {
  x <- cos(cumulative.sector) * epsilon
  y <- sin(cumulative.sector) * epsilon
  segments(0, 0, x, y)
  cumulative.sector <- cumulative.sector + sector.size
}

run.length <- length(toy.example.results[["effective.sample.sizes"]])

animation::saveGIF(
  for(j in seq(1, run.length, by = 1)) {
    weights <- unlist(toy.example.results[["all.weights"]][j])
    particles <- toy.example.results[["all.particles"]][[j]]
    epsilon <- unlist(toy.example.results[["epsilons"]][j])
    PlotParticles(particles, weights, cex = 0.7, particle.max, epsilon)
  }, movie.name = "testDistance.gif", interval = 0.3)


toy.example.configuration.1 <- smcToyExample()
toy.example.results.1 <-
  Smc(
    toy.example.configuration.1,
    max.iterations = 100000,
    alpha = 0.9,
    number.of.particles = 1000,
    number.of.replicates = 1,
    stop.epsilon = 0.01,
    verbose = T
  )


particle.max <- max(abs(unlist(toy.example.results.1[["all.particles"]])))
animation::saveGIF(
  for(j in seq(1, length(toy.example.results.1[["epsilons"]]), by = 1)) {
    weights <- unlist(toy.example.results.1[["all.weights"]][j])
    particles <- toy.example.results.1[["all.particles"]][[j]]
    epsilon <- unlist(toy.example.results.1[["epsilons"]][j])
    PlotParticles(particles, weights, cex = 0.7, particle.max, epsilon)
  }, movie.name = "testDistance_09_1000.gif", interval = 0.3)




toy.example.configuration.rep <- smcToyExample()
toy.example.configuration.rep.results <-
  Smc(
    toy.example.configuration.rep,
    max.iterations = 100000,
    alpha = 0.9,
    number.of.particles = 1000,
    number.of.replicates = 50,
    stop.epsilon = 0.01,
    verbose = T
  )


particle.max <- max(abs(unlist(toy.example.configuration.rep.results[["all.particles"]])))
animation::saveGIF(
  for(j in seq(1, length(toy.example.configuration.rep.results[["epsilons"]]), by = 1)) {
    weights <- unlist(toy.example.configuration.rep.results[["all.weights"]][j])
    particles <- toy.example.configuration.rep.results[["all.particles"]][[j]]
    epsilon <- unlist(toy.example.configuration.rep.results[["epsilons"]][j])
    PlotParticles(particles, weights, cex = 0.7, particle.max, epsilon)
  }, movie.name = "testDistance_09_1000_50.gif", interval = 0.3)


CreateGifAnimation(toy.example.configuration.rep.results, "testDistance_09_1000_50_full.gif", skip.frames = 1)


