
source("SpecificationSims/SimFunctions.R")

# Write a function that takes as given some simulation parameters, and runs these commands a number of
# times so that we can grab average bandwidth values and start to see if there are patterns(which metrics
# suggest simmilar bandwidths, etc)

simulation = function(iteration, DGPparameters) {
  
  Data = DataGen(DGPparameters$sample.size, 
                 DGPparameters$error.sd, 
                 DGPparameters$B1.spatial.var, 
                 DGPparameters$B2.spatial.var)
  
  output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
  new.output = Reorganizer(output)
  simMetrics = LWRMetrics(new.output, Data)
  min.Generator(simMetrics)
  
}
  
sim.parameters = data.frame(sample.size = 50, 
                            error.sd = .5, 
                            B1.spatial.var = 1, 
                            B2.spatial.var = .5)

simulation(1, sim.parameters)

lappy(1:20, simulation, DGPparameters = sim.parameters)

