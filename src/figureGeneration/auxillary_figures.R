library(latex2exp)

# Show example plots for illustrating importance sampling
VectorizedBeta <- Vectorize(function(x) {if(x < 2) { 0 } else {dbeta(x - 2, 2, 5)}})
curve(VectorizedBeta, from = 0, to  = 5, ylim = c(0, 4), col = "red", ann = F)

VectorizedNormal <- Vectorize(function(x) {3 * dnorm(x, 2.3, 0.4)})
curve(VectorizedNormal, add = T, col = "blue")

text = c(latex2exp("p(x)"), latex2exp("g(x)"))
legend("topright", legend = text, lty = c(1, 1), lwd = c(2.5, 2.5), col = c("red", "blue"))