

source("SpecificationSims/SimFunctions.R")

# set our simulation parameters
Replications = 3
sample.size = 20 #c(50, 100, 200, 500)
error.sd = 3 #c(1, 3, 5)
B1.spatial.var = .25 #c(0, .25, .75)
B2.spatial.var = .25 #c(0, .25, .75)

# expand the parameter vectors and create a container for our simulation output
sim.parameters = expand.grid(error.sd, B1.spatial.var, B2.spatial.var, sample.size)
names(sim.parameters) = c("error.sd", "B1.spatial.var", "B2.spatial.var", "sample.size")

meta.sim.num = dim(sim.parameters)[1]

R2Output = array(NA, c(length(sample.size),
                       length(error.sd),
                       length(B1.spatial.var),
                       length(B2.spatial.var),
                       Replications,
                       2),
                     dimnames = list(ss = sample.size,
                                     error.sd = error.sd,
                                     B1sv = B1.spatial.var,
                                     B2sv = B2.spatial.var,
                                     simNum = 1:Replications,
                                     R2 = c("OLS", "LWR")       ))

MetricOutput = array(NA, c(length(sample.size),
                        length(error.sd),
                        length(B1.spatial.var),
                        length(B2.spatial.var),
                        Replications,
                        13, 14),
                  dimnames = list(ss = sample.size,
                                  error.sd = error.sd,
                                  B1sv = B1.spatial.var,
                                  B2sv = B2.spatial.var,
                                  simNum = 1:Replications,
                                  optimized = c("AICc", "corB0", "corB1", "corB2", "CV", "GCV",
                                                "RMSE.B0", "RMSE.B1", "RMSE.B2", "SCV",
                                                "ttest%B0", "ttest%B1", "ttest%B2"),
                                  metric = c("bandwidths", "B0.cor", "B1.cor", "B2.cor",
                                             "B0.RMSE", "B1.RMSE", "B2.RMSE",
                                             "B0.t.perc", "B1.t.perc", "B2.t.perc", "GCV", "SCV", "CV", "AICc")
                  )
)

# now march through the different parameter combinations running the simulations

for( i in 1:meta.sim.num) { 
  start = Sys.time()
  simRepOut = simulationReplicator(Replications, sim.parameters[i, ], MC = FALSE)
  simOut = simRepReorganizer(simRepOut)
  
  R2Output[as.character(sim.parameters[i, "sample.size"]),
           as.character(sim.parameters[i, "error.sd"]),
           as.character(sim.parameters[i, "B1.spatial.var"]),
           as.character(sim.parameters[i, "B2.spatial.var"]), , ] = simOut[[1]]
  
  MetricOutput[as.character(sim.parameters[i, "sample.size"]),
               as.character(sim.parameters[i, "error.sd"]),
               as.character(sim.parameters[i, "B1.spatial.var"]),
               as.character(sim.parameters[i, "B2.spatial.var"]), , , ] = simOut[[2]]
  end = Sys.time()

  print(paste("For loop", i,"of", meta.sim.num))
  print(round(difftime(end, start, units = "m"), 2))
  #save(R2Output, MetricOutput, file = "SpecificationSims/uberScriptOutput.RData")
}
