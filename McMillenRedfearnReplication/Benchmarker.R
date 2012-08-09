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

print(system.time(source("LoopBenchmarker.R")))

print(system.time(simulation.out <- lapply(1:Replications, Simulation)))

require(multicore)
options(cores = 4)
#### NOTE: DO NOT run this line of code within RStudio
print(system.time(simulation.out <- mclapply(1:Replications, Simulation)))


