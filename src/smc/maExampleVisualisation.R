library(latex2exp)


ma.example.configuration3 <- smcMovingAverageExample(use.raw.distance.function = F, create.debug.variables = T)

run.length <- length(ma.example.results3$all.thetas)
all.particles <- ma.example.results3$all.particles
all.thetas <- ma.example.results3$all.thetas

GenerateSample <- ma.example.configuration3[["GenerateSample"]]
time.series <- unlist(GenerateSample(c(theta1, theta2)))
plot(time.series, type = "l")

dim(time.series)


png("test_time_series_plot.png")

first <- T
counter <- 1
for(time.series in all.particles[[run.length]]) {
  # print(dim(unlist(time.series)))

  if(first) {
    plot(unlist(time.series)[1, ], type = "l")
    first <- F
  }
  points(unlist(time.series)[1, ], type = "l")

  # if(counter > 10) {
  # break
  # }
  # counter <- counter + 1
}

dev.off()

theta1 <- theta1.actual
theta2 <- theta2.actual
time.series.length <- 100
sigma.squared <- 1


ComputeMALikelihood <- function(theta1, theta2, time.series.length) {
  B <- -rbind(c(theta1, theta2), c(0, theta1))
  L <- diag(1, nrow = time.series.length)
  diag(L[2:time.series.length, 1:(time.series.length - 1)]) <- -theta1
  diag(L[3:time.series.length, 1:(time.series.length - 2)]) <- -theta2

  F.mat <- rbind(B, matrix(0, nrow = time.series.length - 2, ncol = 2))

  # TODO This is a bit off because of the start of the range
  a.est <- rep(0, time.series.length)
  for(i in 3:time.series.length) {
    a.est[i] <- (time.series[i] + theta1 * a.est[i - 1] + theta2 * a.est[i - 2])^2
  }

  D <- diag(2) + t(F.mat) %*% solve(t(L)) %*% solve(L) %*% F.mat
  sigma.squared^(-time.series.length/2) * det(D)^(-1/2) * exp(-(1/(2 * sigma.squared)) * sum(a.est))

  }




animation::saveGIF(
  for(i in seq(1, run.length, by = 2)) {

    thetas.at.step <- all.thetas[[i]]
    plot(t(thetas.at.step), xlim = c(-2, 2), ylim = c(-1, 1), pch = 16, cex = 0.1, ann = F)
    title(main = "Likelihood", xlab = latex2exp("$\\theta_{1}$"), ylab = latex2exp("$\\theta_{2}$"))
  }, movie.name = "likelihood_update", interval = 0.3)


