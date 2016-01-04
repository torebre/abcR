

ComputeEffectiveSampleSize <- function(weights) {
  1 / sum(weigths^2)
}