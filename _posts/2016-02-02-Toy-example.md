---
layout: post
title:  "Toy example from article"
---

Distribution used in toy example:
$$
p(x|\theta)\sim\frac{1}{2}N(\theta,1)+\frac{1}{2}N(\theta,\frac{1}{100})
$$

$$
\theta\sim Uniform(-10,10)
$$

Running a toy experiment as in the article with $$\alpha=0.9$$. Only one MH-step per iteration.

![Density]({{ site.baseurl }}/fig/toyExample/density.png)

![Effective Sample Size]({{ site.baseurl }}/fig/toyExample/eff.png)

![Epsilon]({{ site.baseurl }}/fig/toyExample/epsilon.png)

## Example of evolution of a particle
![Theta]({{ site.baseurl }}/fig/toyExample/theta_index.png)

![Weight]({{ site.baseurl }}/fig/toyExample/weight_index.png)

![Sample]({{ site.baseurl }}/fig/toyExample/sample_index.png)
