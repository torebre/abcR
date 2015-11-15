





indices.ordered.by.distance <- order(abc.distance.parameters[, 1])


ordered.parameter.matrix <- abc.distance.parameters[indices.ordered.by.distance, ]


cumulative.mean.estimates <- matrix(0, length(abc.samples), length(observations))
mspe.k <- rep(NA, length(abc.samples))


test <- abc.samples[[indices.ordered.by.distance[1]]]$abc.prior
test[y.coords]


for(i in 1:length(abc.samples)) {
  kth.sample <- abc.samples[[indices.ordered.by.distance[i]]]$abc.prior

  if(i > 1) {
    prev.sum <- (i - 1) * cumulative.mean.estimates[i - 1]
  }
  else {
    prev.sum <- 0
  }
  
  cumulative.mean.estimates[i, ] <- (prev.sum + kth.sample[y.coords]) / i
  
  mspe.k[i] <- sum((observations - cumulative.mean.estimates[i , ])^2)
}



plot(mspe.k, type = "l")

plot(mspe.k[1:50], type = "l")


which.min(mspe.k)






