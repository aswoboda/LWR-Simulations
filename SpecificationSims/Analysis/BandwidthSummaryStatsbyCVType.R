load("~/LWR-Simulations/SpecificationSims/SimulationOutput.RData")

sim.parameters = expand.grid(MetricOutput["ss"], error.sd, B1.spatial.var, B2.spatial.var)
names(sim.parameters) = c("sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var")

sim.parameters = expand.grid(sample.size = as.numeric(dimnames(MetricOutput)$ss), 
                             error.sd = as.numeric(dimnames(MetricOutput)$error.sd),
                             B1.spatial.var =  as.numeric(dimnames(MetricOutput)$B1sv), 
                             B2.spatial.var =  as.numeric(dimnames(MetricOutput)$B2sv))


sim.parameters$GCV.mean = NA
sim.parameters$GCV.sd = NA
sim.parameters$SCV.mean = NA
sim.parameters$SCV.sd = NA
sim.parameters$CV.mean = NA
sim.parameters$CV.sd = NA
for (i in 1: dim(sim.parameters)[1]) {
  #i = 1
  temp = (MetricOutput[as.character(sim.parameters[i, "sample.size"]),
                       as.character(sim.parameters[i, "error.sd"]),
                       as.character(sim.parameters[i, "B1.spatial.var"]),
                       as.character(sim.parameters[i, "B2.spatial.var"]), , "GCV", "bandwidths"])
  sim.parameters[i,"GCV.mean"] = mean(temp)
  sim.parameters[i,"GCV.sd"] = sd(temp)
  temp = (MetricOutput[as.character(sim.parameters[i, "sample.size"]),
                       as.character(sim.parameters[i, "error.sd"]),
                       as.character(sim.parameters[i, "B1.spatial.var"]),
                       as.character(sim.parameters[i, "B2.spatial.var"]), , "SCV", "bandwidths"])
  sim.parameters[i,"SCV.mean"] = mean(temp)
  sim.parameters[i,"SCV.sd"] = sd(temp)
  temp = (MetricOutput[as.character(sim.parameters[i, "sample.size"]),
                       as.character(sim.parameters[i, "error.sd"]),
                       as.character(sim.parameters[i, "B1.spatial.var"]),
                       as.character(sim.parameters[i, "B2.spatial.var"]), , "CV", "bandwidths"])
  sim.parameters[i,"CV.mean"] = mean(temp)
  sim.parameters[i,"CV.sd"] = sd(temp)
  
}

