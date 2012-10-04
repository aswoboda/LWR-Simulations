

source("SpecificationSims/SimFunctions.R")

# set our simulation parameters
Replications = 10
sample.size = c(50, 100, 200)
error.sd = c(2, 4, 6)
B1.spatial.var = c(0, .1, .2, .3)
B2.spatial.var = c(0, .1, .2, .3)

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
                        # Be careful about the dimension, must be ((number -1) of metrics 
                           #in LWRMetrics function output in SimFunctions (1 per line), metrics)
                           14, 15),
                  dimnames = list(ss = sample.size,
                                  error.sd = error.sd,
                                  B1sv = B1.spatial.var,
                                  B2sv = B2.spatial.var,
                                  simNum = 1:Replications,
                                  # IMPORTANT: Before running uberscript, test it once to make sure the labels are in
                                  # the proper order!
                                  optimized = c( "AICc", "corB0", "corB1", "corB2", "CV", "GCV", "R2",
                                                "RMSE.B0", "RMSE.B1", "RMSE.B2", "SCV",
                                                "ttest%B0", "ttest%B1", "ttest%B2"),
                                  metric = c("bandwidths", "B0.cor", "B1.cor", "B2.cor",
                                             "B0.RMSE", "B1.RMSE", "B2.RMSE",
                                             "B0.t.perc", "B1.t.perc", "B2.t.perc", "GCV", "SCV", "CV", "AICc", "R2")
                  )
)

# now march through the different parameter combinations running the simulations

for( i in 1:10) { #meta.sim.num
  start = Sys.time()
  simRepOut = simulationReplicator(Replications, sim.parameters[i, ], MC = TRUE)
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
  save(R2Output, MetricOutput, file = "SpecificationSims/Data/uberScriptOutput.RData")
}
