To Do
========================================================

This is a place to keep track of future ideas, concerns, and tasks that need to be thought about/taken care of.


Right now we have our location variables uniformly over the range [0, 10] and then the betas are a linear function of east and north. However, would it make more sense to have the location variabes vary symmetrically about zero? It doesn't matter what the coefficient is if your variable is zero, so I'm worried that the distribution of y is going to be very different for the different degrees of spatial variation in the betas. *I should run a simulation of the DGP and look at the distribution of y. Then, try to change the x values to see if I can get the same distribution? Mean? SD?

I should rerun some code with the new server structure to see how much faster it will run.