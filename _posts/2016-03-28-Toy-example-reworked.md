---
layout: post
title:  "Toy example reworked"
---

Below is an animation showing the SMC-algorithm for the toy example in Del Moral's article (originally from an article by Sisson et al). Here only one particle is used for each $$\theta$$. Because of this the weights of the particles are either 0 or 1. The graph to the upper right shows the distances between samples from the distribution and the observed value which is 0 (so the statistic is $$\mathrm{abs}(x - 0)$$). With this statistic the sample distribution converges towards the true distribution.

Distribution used in toy example:
$$
p(x|\theta)\sim\frac{1}{2}N(\theta,1)+\frac{1}{2}N(\theta,\frac{1}{100})
$$

The prior is uniform.

![Toy example]({{ site.baseurl }}/fig/smc_toy_example.gif)

The green outline in the graph to the upper right is the true distribution. The red dots in the graph on the upper right are the particles with weight 1, and the blue ones with weight 0. The unweighted are allowed to move around until a resampling occurs, when they are replaced. The weighted particles get closer and closer to 0 $$\epsilon$$ decreases.
