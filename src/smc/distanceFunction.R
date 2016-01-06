


DistanceFunction <- function(my.sample, my.observed.sample) {
  mean(my.sample) - mean(my.observed.sample)
}