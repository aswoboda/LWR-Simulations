# empty sandbox


source("SpecificationSims/SimFunctions.R")

# set our simulation parameters
Replications = 10
sample.size = c(50, 100, 200, 500, 1000, 2000)
error.sd = c(3)
B1.spatial.var = 0.25
B2.spatial.var = 0

sim.parameters = expand.grid(sample.size, error.sd, B1.spatial.var, B2.spatial.var)
names(sim.parameters) = c("sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var")

meta.sim.num = dim(sim.parameters)[1]

DGPparameters = sim.parameters[1, ]



simulation(1, DGPparameters)



simRepOut = simulationReplicator(N = 3, DGPparameters)



simOut = simRepReorganizer(simRepOut)

new.array = array(NA, c(2, 3, 12, 13),
                  dimnames = list(temp = 1:2,
                                  replication = dimnames(simOut[[2]])[[1]],
                                  optimized = dimnames(simOut[[2]])[[2]], 
                                  metric.values = dimnames(simOut[[2]])[[3]]) )
dimnames(new.array)

new.array[1, , , ] = simOut[[2]]
### Problem - right now we are just grabbing the bandwidths that optimize different metrics, 
### we also want to grab the metric values at each of these "optimal" bandwidths to see how they compare

# instead of outputting a vector of bandwidths, will we want a matrix/array?


[1] "For loop 1 of 4"
Time difference of 0.08 mins
[1] "For loop 2 of 4"
Time difference of 0.3 mins
[1] "For loop 3 of 4"
Time difference of 1.37 mins
[1] "For loop 4 of 4"
Time difference of 11.62 mins
> 
  > proc.time()
user   system  elapsed 
1329.587    0.852  802.615 











