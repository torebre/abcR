

epsilon <- 0.001
counter <- 1
samples <- rep(NA, 15000)

while(T) {
  temp <- runif(1, min = -3, max = 3)
  x <- GenerateSample(temp)
  if(DistanceFunction(x) < epsilon) {
    samples[counter] <- temp
    if(counter == 15000) {
      break;
    }
    print(paste("Got sample: ", counter))
    counter <- counter + 1
  }
}

hist(samples, breaks = seq(from = -3, to = 3, by = 0.1), freq = F, ylim = c(0, 2.5), ann = F)
# hist(temp)

# Prior is uniform, and is not included here
Posterior <- function(my.theta) {
  (pnorm(epsilon - my.theta) - pnorm(-(epsilon + my.theta)) + pnorm(10 * (epsilon - my.theta))) - pnorm(-10 * (epsilon + my.theta))
}

VectorizedPosterior <- Vectorize(Posterior)
curve(VectorizedPosterior,
      from = -3,
      to = 3)


test.function <- function() {0.5 * rnorm(1, mean = 0, sd = 1) + 0.5 * rnorm(1, mean = 0, sd = 1 / 10)}
test.samples <- replicate(5000, test.function())
truehist(test.samples)


Posterior2 <- function(my.theta) {
  0.5 * dnorm(0, mean = my.theta, sd = 1) + 0.5 * dnorm(0, mean = my.theta, sd = 1 / 10)
}
VectorizedPosterior2 <- Vectorize(Posterior2)
curve(VectorizedPosterior2,
      from = -3,
      to = 3, add = T)



test.function <- function() {if(runif(1) < 0.5) {rnorm(1, mean = 0, sd = 1) } else {0.5 * rnorm(1, mean = 0, sd = 1 / 10)}}
test.samples <- replicate(5000, test.function())
truehist(test.samples)



