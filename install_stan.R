dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXXFLAGS=-O3", file = M, sep = "\n", append = TRUE)

install.packages("rstan", dependencies = TRUE)



library(rstan) # observe startup messages


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())