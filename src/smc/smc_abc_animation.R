library(animation)
library(latex2exp)

## make sure ImageMagick has been installed in your system
saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
})
## if the above conversion was successful, the option

saveGIF({
  brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, movie.name = "brownian_motion.gif", interval = 0.1, nmax = 30, ani.width = 600,
ani.height = 600)
## non-constant intervals between image frames
saveGIF({
  brownian.motion(pch = 21, cex = 5, col = "red", bg = "yellow")
}, movie.name = "brownian_motion2.gif", interval = runif(30, 0.01, 1), nmax = 30)



saveGIF({
  for(i in 1:39) {
  filled.contour(phi.points, variance.points, post.prob.eval.points.matrix,
                 plot.axes = points(all.thetas[[i]][, 1], all.thetas[[i]][, 2], col = 'red', pch = 19, cex = 0.3))
    title(latex2exp('Likelihood $p(x | \\theta)$'))
  }
  
}, movie.name = "smb_abc.gif", interval = 0.2)



