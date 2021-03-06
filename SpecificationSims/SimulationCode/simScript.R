

source("SpecificationSims/SimFunctions.R")

# write a function that takes as given some simulation parameters, and runs the these commands a number of times
# so that we can start to see if there are patterns (which metrics suggest similar bandwidths, etc.)
sim.parameters = data.frame(sample.size = 30, 
                            error.sd = .5,
                            B1.spatial.var = 1,
                            B2.spatial.var = .5)

start = Sys.time()
simData = simulationReplicator(2, sim.parameters, MC=FALSE)
end = Sys.time()
print(end-start)

simulationOutput = list(sim.parameters, simData)

save(simulationOutput, file = "SpecificationSims/SimulationResults.RData")