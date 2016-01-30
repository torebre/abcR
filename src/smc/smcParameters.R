# Number of particles
kNumberOfParticles <- 10

# Number of replicates
kNumberOfReplicates <- 1


NormalizeVector <- function(my.vector) {
  my.vector.sum <- sum(my.vector)
  my.vector / my.vector.sum
  
#   my.vector.min <- min(my.vector)
#   my.vector.range <- max(my.vector) - my.vector.min 
#   # TODO Have some other cutoff criteria?
#   if(my.vector.range == 0) {
#     return(rep(1 / length(my.vector), length(my.vector)))
#   }
#   sapply(my.vector, function(x) {
#     (x - my.vector.min) / my.vector.range
#   })
}

