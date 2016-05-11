---
layout: post
title:  "MA example"
---

Below is an animation showing the use of the ABC SMC-method on an example used in "Approximate Bayesian computational methods" by Marin et. al.

The example uses ABC to estimate $$\theta_{1}$$ and $$\theta_{2}$$ in an MA(2) model. The time series has length 100.

A closed expression for the likelihood exists so the results can be compared with that (the prior is uniform).

![MA example]({{ site.baseurl }}/fig/likelihood_update.gif)

There are a number of bugs present, and I think I have messed up some + and - somewhere, and made some tweaks to visualisation to cancel that, but overall it looks like the particles are converging to what is expected.
