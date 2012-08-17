

source("SpecificationSims/SimFunctions.R")

# set our simulation parameters
Replications = 150
sample.size = c(50, 100, 200)
error.sd = c(1, 3, 5)
B1.spatial.var = c(0, .25, .75)
B2.spatial.var = c(0, .25)

# expand the parameter vectors and create a container for our simulation output
sim.parameters = expand.grid(error.sd, B1.spatial.var, B2.spatial.var, sample.size)
names(sim.parameters) = c("error.sd", "B1.spatial.var", "B2.spatial.var", "sample.size")

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
  simOutput[as.character(sim.parameters[i, "sample.size"]),
            as.character(sim.parameters[i, "error.sd"]),
            as.character(sim.parameters[i, "B1.spatial.var"]),
            as.character(sim.parameters[i, "B2.spatial.var"]), , ] = as.matrix(simulationReplicator(Replications, sim.parameters[i, ], MC = TRUE))
  end = Sys.time()

  print(paste("For loop", i,"of", meta.sim.num))
  print(difftime(end, start, units = "h"))
  #save(simOutput, file = "SpecificationSims/uberScriptOutput.RData")
}
