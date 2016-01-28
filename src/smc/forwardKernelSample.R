




test.list <- list()
test.list[[1]] <- c(1, 2)
test.list[[2]] <- c(3, 4)


test.matrix <- sapply(test.list, function(x) {x})
dim(test.matrix) <- NULL
var(test.matrix)
