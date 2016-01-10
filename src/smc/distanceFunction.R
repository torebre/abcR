


DistanceFunction <- function(my.sample, my.observed.sample) {
  abs(mean(my.sample) - mean(my.observed.sample))
}