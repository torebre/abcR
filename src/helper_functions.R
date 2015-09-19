# Functions for calculating the covariance
CovarianceFunction <- function(distance) {
  kVariance * exp(-distance / kPhi)
}

CalculateCovariance <-
  function(x1.coord, y1.coord, x2.coord, y2.coord)  {
    CovarianceFunction(sqrt((x1.coord - y1.coord)^2 + (x2.coord - y2.coord)^2))
  }
