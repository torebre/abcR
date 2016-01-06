

ComputeEffectiveSampleSize <- function(weights) {
  1 / sum(weights^2)
}