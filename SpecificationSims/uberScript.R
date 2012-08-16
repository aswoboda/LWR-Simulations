

source("SpecificationSims/SimFunctions.R")

# set our simulation parameters
Replications = 10
sample.size = c(50, 100, 200)
error.sd = c(.3, 1, 3)
B1.spatial.var = c(0, .25, .75)
B2.spatial.var = 0

# expand the parameter vectors and create a container for our simulation output
sim.parameters = expand.grid(sample.size, error.sd, B1.spatial.var, B2.spatial.var)
names(sim.parameters) = c("sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var")

meta.sim.num = dim(sim.parameters)[1]

simOutput = array(NA, c(length(sample.size),
                        length(error.sd),
                        length(B1.spatial.var),
                        length(B2.spatial.var),
                        Replications,
                        14),
                  dimnames = list(ss = sample.size,
                                  error.sd = error.sd,
                                  B1sv = B1.spatial.var,
                                  B2sv = B2.spatial.var,
                                  simNum = 1:Replications,
                                  metric = c("GCV", "SCV", "SSRB0", "SSRB1", "SSRB2", 
                                             "ttest%B0", "ttest%B1", "ttest%B2", 
                                             "corB0", "corB1", "corB2", "corDepVar",
                                             "R2OLS", "R2LWR")
                  )
)

# now march through the different parameter combinations running the simulations

for( i in 1:meta.sim.num) { 
  start = Sys.time()
  simOutput[as.character(sim.parameters[i, 1]),
            as.character(sim.parameters[i, 2]),
            as.character(sim.parameters[i, 3]),
            as.character(sim.parameters[i, 4]), , ] = as.matrix(simulationReplicator(Replications, sim.parameters[i, ], MC = TRUE))
  end = Sys.time()
  mytime = end - start
  print(paste("For loop", i,"of", meta.sim.num))
  print(mytime)
  save(simOutput, file = "SpecificationSims/uberScriptOutput.RData")
}
