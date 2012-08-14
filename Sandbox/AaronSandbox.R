


source("SpecificationSims/SimFunctions.R")

# write a function that takes as given some simulation parameters, and runs the these commands a number of times
# so that we can start to see if there are patterns (which metrics suggest similar bandwidths, etc.)
Replications = 2
sample.size = c(20, 100)
error.sd = c(.3, 1)
B1.spatial.var = c(.5, 1)
B2.spatial.var = 1

sim.parameters = expand.grid(sample.size, error.sd, B1.spatial.var, B2.spatial.var)
names(sim.parameters) = c("sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var")

simOutput = array(NA, c(length(sample.size),
                        length(error.sd),
                        length(B1.spatial.var),
                        length(B2.spatial.var),
                        Replications,
                        11),
                  dimnames = list(ss = sample.size,
                                  error.sd = error.sd,
                                  B1sv = B1.spatial.var,
                                  B2sv = B2.spatial.var,
                                  simNum = 1:Replications,
                                  metric = c("GCV", "SCV", "SSRB0", "SSRB1", "SSRB2", 
                                             "ttest%B0", "ttest%B1", "ttest%B2", "corB1",
                                             "corB2", "corDepVar")
                                  )
                  )


simData = simulationReplicator(Replications, sim.parameters[1, ], MC=FALSE)

simOutput[as.character(sim.parameters[1, 1]),
          as.character(sim.parameters[1, 2]),
          as.character(sim.parameters[1, 3]),
          as.character(sim.parameters[1, 4]), , ] = as.matrix(simData)

simOutput

# test.array = array(NA, c(2, 3, 4),
#                    dimnames = list(dim1 = letters[1:2],
#                                    dim2 = LETTERS[1:3],
#                                    dim3 = 1:4))
