---
layout: post
title:  "MA experiments"
---

More animations related to the MA(2) example.

Animated results when looking at autocovariance distances:
![MA example autocovariance distance]({{ site.baseurl }}/fig/likelihood_update2.gif)

Results when using raw distances (expected that this should give worse results than when using the autocovariances):
![MA example raw distance]({{ site.baseurl }}/fig/likelihood_update_raw_distance.gif)

Setting $$\alpha=0.99$$ to see if it will prevent the algorithm from collapsing:
![MA example raw distance 2]({{ site.baseurl }}/fig/likelihood_update_raw_distance_099.gif)

Some graphs that might be used for explaining what is happening when the algorithm is not able to make progress on $$\epsilon$$ any longer.

Here with $$\alpha = 0.9$$:

![MA example raw distances]({{ site.baseurl }}/fig/raw_distances.gif)

Here with $$\alpha = 0.99$$:

![MA example raw distances 0.99]({{ site.baseurl }}/fig/raw_distances_2.gif)
