#' MA(2) example.
#'
#' @export
smcMovingAverageExample <- function(create.debug.variables = F) {
  maExample <- structure(list(), class = "smcConfiguration")
  theta1.actual <- 0.6
  theta2.actual <- 0.2
  series.length <- 100


  GenerateSample <- function(my.theta) {
    mu.values <- rnorm(series.length + 2, mean = 0, sd = 1)
    simulated.series <- rep(NA, series.length)

    sapply(1:series.length, function(x) {
      mu.values[x] + my.theta[1] * mu.values[x + 1] + my.theta[2] * mu.values[x + 2]
    })
  }

  observed.series <- GenerateSample(c(theta1.actual, theta2.actual))

  maExample[["GenerateRandomPrior"]] <- function() {
    # -2 < theta1 < 2, theta1 + theta2 > -1, theta1 - theta2 < 1
    theta1 <- runif(1, min = -2, max = 2)

    while(T) {
      theta2 <- runif(1, min = -1, max = 1)
      if(theta1 + theta2 > -1 && theta1 - theta2 < 1)
        return(c(theta1, theta2))
    }
  }

  EvaluateTheta <- function() {
    # The prior is uniform on a triangle with width 4 and height 2
    1 / 4
  }

  InternalDistanceFunction <- function(my.sample) {
    abs(my.sample - observation)
  }

  maExample[["DistanceFunction"]] <- InternalDistanceFunction

  EvaluateLikelihoodSum <-
    function(my.sample.vector, my.current.epsilon) {
      sum(sapply(my.sample.vector, function(x) {
        #       print(paste("x: ", x))
        #       print(paste("Distance: ", DistanceFunction(x)))
        #       print(paste("my.current.epsilon: ", my.current.epsilon))

        InternalDistanceFunction(x) < my.current.epsilon
      }))
    }

  debug.variables <- new.env(parent = emptyenv())
  debug.variables$avg.acc.rate <- list()
  debug.variables$empirical.variance <- list()
  debug.variables$alive.particles <- list()
  debug.variables$accepted <- list()

  maExample[["GetDebugVariables"]] <- function() {
    debug.variables
  }


  maExample[["ForwardKernelSample"]] <- function(samples.old,
                                                  theta.old,
                                                  my.current.epsilon,
                                                  my.weights) {
    temp1 <- sapply(theta.old, function(x) { x[1] })
    temp2 <- sapply(theta.old, function(x) { x[2] })

    empirical.variance1 <- var(temp1)
    empirical.variance2 <- var(temp2)

    my.number.of.particles <- length(samples.old)
    my.number.of.replicates <- length(samples.old[[1]])[2]

    samples.new <- rep(samples.old)
    theta.new <- rep(theta.old)

    accepted <- 0
    alive.particles <- 0

    for (j in 1:my.number.of.particles) {
      theta.candidate <- c(rnorm(1, mean = theta.old[j, 1], sqrt(2 * empirical.variance1)),
                           rnorm(1, mean = theta.old[j, 2], sqrt(2 * empirical.variance2)))


      if(create.debug.variables) {
        debug.variables$empirical.variance[[length(debug.variables$avg.acc.rate) + 1]] <-
          sqrt(2 * empirical.variance)
      }

      replicates.new <- matrix(nrow = my.number.of.replicates, ncol = series.length)

      for (i in 1:my.number.of.replicates) {
        replicates.new[i, ] <- GenerateSample(theta.candidate)
      }

      # The prior is uniform and the random walk is coming from
      # a symmetric distribution so the only term left in the
      # Metropolis-Hastings ratio is the likelihood

      if (my.weights[j] == 0) {

        # Try to see what happens when the particles with no
        # weight are left alone

        # theta.new[j] <- theta.candidate
        # samples.new[[j]] <- replicates.new


        # accepted <- accepted + 1
      }
      else {
        # New as nominator, old as denominator
        old.likelihood <-
          EvaluateLikelihoodSum(samples.new[j], my.current.epsilon)

        # auto.accept <- false
        if (old.likelihood == 0) {
          next
        }

        metropolis.hastings.ratio <-
          EvaluateLikelihoodSum(replicates.new, my.current.epsilon) / old.likelihood

        #       print(paste("New: ", EvaluateLikelihoodSum(replicates.new, my.current.epsilon)))
        #       print(paste("Old: " ,EvaluateLikelihoodSum(samples.old[j], my.current.epsilon)))

        if (runif(1) <= min(1, metropolis.hastings.ratio)) {
          theta.new[j] <- theta.candidate
          samples.new[[j]] <- replicates.new

          accepted <- accepted + 1
        }

        alive.particles <- alive.particles + 1

      }

      # }

    }

    if(create.debug.variables) {
      debug.variables$accepted[[length(debug.variables$accepted) + 1]] <-
        accepted
      debug.variables$avg.acc.rate[[length(debug.variables$avg.acc.rate) + 1]] <-
        accepted / alive.particles
      debug.variables$alive.particles[[length(debug.variables$alive.particles) + 1]] <-
        alive.particles
    }

    return(list(theta = theta.new, samples = samples.new))
  }


    SampleFunctionInternal <- function(my.thetas, my.number.of.replicates) {
      my.samples <- list()
      for (i in 1:length(my.thetas)) {
        my.replicates <- matrix(nrow = my.number.of.replicates, ncol = series.length)
        for (j in 1:my.number.of.replicates) {
          my.replicates[j, ] <- GenerateSample(my.thetas[[i]])
        }
        my.samples[[i]] <- my.replicates
      }
      return(my.samples)
    }

    maExample[["SampleFunction"]] <- SampleFunctionInternal


  maExample
}
