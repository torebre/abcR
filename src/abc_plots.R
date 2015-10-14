postfix <- "_abcexp4"
fig.dir <- "abcexp4"


# x given y

# Phi
png(paste("../../abcR_doc/fig/", fig.dir, "/x_given_y_dist_phi", postfix, ".png", sep = ""))
plot(abc.distance.parameters[ , 2], abc.distance.parameters[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\phi$'), ylab = latex2exp('$d(s(x),s(y))$'))
dev.off()

# Mean
png(paste("../../abcR_doc/fig/", fig.dir, "/x_given_y_dist_mean", postfix, ".png", sep = ""))
plot(abc.distance.parameters[ , 3], abc.distance.parameters[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\mu$'), ylab = latex2exp('$d(s(x),s(y))$'))
dev.off()

# Variance
png(paste("../../abcR_doc/fig/", fig.dir, "/x_given_y_dist_var", postfix, ".png", sep = ""))
plot(abc.distance.parameters[ , 4], abc.distance.parameters[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\sigma^{2}$'), ylab = latex2exp('$d(s(x),s(y))$'))
dev.off()

# Observation noise
png(paste("../../abcR_doc/fig/", fig.dir, "/x_given_y_dist_obs_noise", postfix, ".png", sep = ""))
plot(abc.distance.parameters[ , 5], abc.distance.parameters[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\nu$'), ylab = latex2exp('$d(s(x),s(y))$'))
dev.off()


# x given y average

# Phi
png(paste("../../abcR_doc/fig/", fig.dir, "/x_given_y_dist_phi_avg", postfix, ".png", sep = ""))
plot(abc.distance.parameters.avg[ , 2], abc.distance.parameters.avg[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\phi$'), ylab = latex2exp('$d(s(x),s(y))$'))
dev.off()

# Mean
png(paste("../../abcR_doc/fig/", fig.dir, "/x_given_y_dist_mean_avg", postfix, ".png", sep = ""))
plot(abc.distance.parameters.avg[ , 3], abc.distance.parameters.avg[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\mu$'), ylab = latex2exp('$d(s(x),s(y))$'))
dev.off()

# Variance
png(paste("../../abcR_doc/fig/", fig.dir, "/x_given_y_dist_var_avg", postfix, ".png", sep = ""))
plot(abc.distance.parameters.avg[ , 4], abc.distance.parameters.avg[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\sigma^{2}$'), ylab = latex2exp('$d(s(x),s(y))$'))
dev.off()


# Observation noise
png(paste("../../abcR_doc/fig/", fig.dir, "/x_given_y_dist_obs_noise_avg", postfix, ".png", sep = ""))
plot(abc.distance.parameters.avg[ , 5], abc.distance.parameters.avg[ , 1], type = "p", pch = 19, cex = 0.5,
     xlab = latex2exp('$\\nu$'), ylab = latex2exp('$d(s(x),s(y))$'))
dev.off()