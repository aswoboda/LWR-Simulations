######
# working directory should be "McMillenRedfearnReplication"
# setwd("McMillenRedfearnReplication/")
source("SimulationFunctions.R")

Replications = 10 # number of times we regenerate a dataset for the simulation

require(multicore)
simulation.out = mclapply(1:Replications, Simulation, 
                        Total.observations = 2000, 
                        Error.sd = .3, 
                        Bandwidths = c(.1, .4)   )