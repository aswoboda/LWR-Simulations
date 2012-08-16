# empty sandbox


source("SpecificationSims/SimFunctions.R")

# set our simulation parameters
Replications = 500
sample.size = c(50, 100, 200, 500, 1000, 2000)
error.sd = c(3)
B1.spatial.var = 0.25
B2.spatial.var = 0

sim.parameters = expand.grid(sample.size, error.sd, B1.spatial.var, B2.spatial.var)
names(sim.parameters) = c("sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var")

meta.sim.num = dim(sim.parameters)[1]

DGPparameters = sim.parameters[1, ]
    

simulation(1, DGPparameters)

