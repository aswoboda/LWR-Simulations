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
sim.parameters$ttest.GCV.SCV = NA
sim.parameters$ttest.GCV.CV = NA
sim.parameters$ttest.SCV.CV = NA

for (i in 1: dim(sim.parameters)[1]) {
  #i = 1:3
  
  temp1 = (MetricOutput[as.character(sim.parameters[i, "sample.size"]),
                       as.character(sim.parameters[i, "error.sd"]),
                       as.character(sim.parameters[i, "B1.spatial.var"]),
                       as.character(sim.parameters[i, "B2.spatial.var"]), , "GCV", "bandwidths"])
  sim.parameters[i,"GCV.mean"] = mean(temp1)
  sim.parameters[i,"GCV.sd"] = round(sd(temp), 2)
  temp2 = (MetricOutput[as.character(sim.parameters[i, "sample.size"]),
                       as.character(sim.parameters[i, "error.sd"]),
                       as.character(sim.parameters[i, "B1.spatial.var"]),
                       as.character(sim.parameters[i, "B2.spatial.var"]), , "SCV", "bandwidths"])
  sim.parameters[i,"SCV.mean"] = mean(temp2)
  sim.parameters[i,"SCV.sd"] = round(sd(temp2), 2)
  temp3 = (MetricOutput[as.character(sim.parameters[i, "sample.size"]),
                       as.character(sim.parameters[i, "error.sd"]),
                       as.character(sim.parameters[i, "B1.spatial.var"]),
                       as.character(sim.parameters[i, "B2.spatial.var"]), , "CV", "bandwidths"])
  sim.parameters[i,"CV.mean"] = mean(temp3)
  sim.parameters[i,"CV.sd"] = round(sd(temp3), 2)
 
  t.result = t.test(temp1, temp2, paired = TRUE)
  sim.parameters[i, "ttest.GCV.SCV"] = round(t.result$p.value, 2)
  t.result = t.test(temp1, temp3, paired = TRUE)
  sim.parameters[i, "ttest.GCV.CV"] = round(t.result$p.value, 2)
  t.result = t.test(temp2, temp3, paired = TRUE)
  sim.parameters[i, "ttest.SCV.CV"] = round(t.result$p.value, 2)
}


