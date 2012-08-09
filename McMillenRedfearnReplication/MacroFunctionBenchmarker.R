######
######
# working directory should be "McMillenRedfearnReplication"
# setwd("McMillenRedfearnReplication/")
source("SimulationFunctions.R")

# Set some parameters for the simulation
Total.observations = 2000 # number of observations in the dataset to be analyzed
Error.sd = .3 # standard deviation of error in datra generating process
Bandwidths = c( .1, .4) # proportion of data receiving positives weights in LWR 

Replications = 10 # number of times we regenerate a dataset for the simulation

simulation.out = lapply(1:Replications, Simulation, 
                        Total.observations = Total.observations, 
                        Error.sd = Error.sd, 
                        Bandwidths = Bandwidths   )
