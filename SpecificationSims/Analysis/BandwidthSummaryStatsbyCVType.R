load("~/LWR-Simulations/SpecificationSims/SimulationOutput.RData")

# Creates matrix of all possible parameter combinations
sim.parameters = expand.grid(MetricOutput["ss"], error.sd, B1.spatial.var, B2.spatial.var)
names(sim.parameters) = c("sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var")

sim.parameters = expand.grid(sample.size = as.numeric(dimnames(MetricOutput)$ss), 
                             error.sd = as.numeric(dimnames(MetricOutput)$error.sd),
                             B1.spatial.var =  as.numeric(dimnames(MetricOutput)$B1sv), 
                             B2.spatial.var =  as.numeric(dimnames(MetricOutput)$B2sv))

# Adds new metrics onto the matrix
sim.parameters$GCV.mean = NA
sim.parameters$GCV.sd = NA
sim.parameters$SCV.mean = NA
sim.parameters$SCV.sd = NA
sim.parameters$CV.mean = NA
sim.parameters$CV.sd = NA
sim.parameters$ttest.GCV.SCV = NA
sim.parameters$ttest.GCV.CV = NA
sim.parameters$ttest.SCV.CV = NA

# Inserts metrics values into the row with the corresponding combination of parameter values
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
# Transforms the data.frame from short to long, have to do mean and sd seperately, then merge.
library(reshape2)
sim.parameters$combo = factor(c(1:108))

templong = melt(sim.parameters, id.vars = c("combo", "sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var"), 
                measure.vars = c("GCV.mean", "SCV.mean", "CV.mean"), value.name = "mean")

levels(templong$variable) = c("GCV", "SCV", "CV")

templong2 = melt(sim.parameters, id.vars = c("combo", "sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var"), 
                 measure.vars = c("GCV.sd", "SCV.sd", "CV.sd"), value.name = "sd")

levels(templong2$variable) = c("GCV", "SCV", "CV")

bandwidth.sum.statsL = merge(templong, templong2)


save(bandwidth.sum.statsL, file = "SpecificationSims/Figures/BandwidthSumStatsLong.RData")

# Now for linear regressions to detect patterns, relationships, etc

# linear regressions for mean
lin1.mean = lm( bandwidth.sum.statsL$mean ~ bandwidth.sum.statsL$sample.size + bandwidth.sum.statsL$error.sd + 
  bandwidth.sum.statsL$B1.spatial.var + bandwidth.sum.statsL$B2.spatial.var)

# with interaction between sample.size and error.sd - interaction is sig, but error.sd is not
lin1.meani.ss.esd = lm(formula = bandwidth.sum.statsL$mean ~ bandwidth.sum.statsL$sample.size + 
  bandwidth.sum.statsL$error.sd + bandwidth.sum.statsL$B1.spatial.var + 
  bandwidth.sum.statsL$B2.spatial.var + bandwidth.sum.statsL$sample.size:bandwidth.sum.statsL$error.sd)

# with interaction between sample.size and B1sv, interaction term is sig, B1sv is not
lin1.meani.ss.B1 = lm(formula = bandwidth.sum.statsL$mean ~ bandwidth.sum.statsL$sample.size + 
  + bandwidth.sum.statsL$error.sd + bandwidth.sum.statsL$B1.spatial.var + 
  + bandwidth.sum.statsL$B2.spatial.var + bandwidth.sum.statsL$sample.size:bandwidth.sum.statsL$B1.spatial.var)

# with interaction between error.sd and B1sv, everything sig at .01
lin1.meani.esd.B1 = lm(formula = bandwidth.sum.statsL$mean ~ bandwidth.sum.statsL$sample.size + 
  + bandwidth.sum.statsL$error.sd + bandwidth.sum.statsL$B1.spatial.var + 
  bandwidth.sum.statsL$B2.spatial.var + bandwidth.sum.statsL$error.sd:bandwidth.sum.statsL$B1.spatial.var)



# linear regressions for sd
lin1.sd = lm( bandwidth.sum.statsL$sd ~ bandwidth.sum.statsL$sample.size + bandwidth.sum.statsL$error.sd + 
  bandwidth.sum.statsL$B1.spatial.var + bandwidth.sum.statsL$B2.spatial.var)

# with interaction between sample.size and error.sd
lin1.sdi.ss.esd = lm(formula = bandwidth.sum.statsL$sd ~ bandwidth.sum.statsL$sample.size + 
  bandwidth.sum.statsL$error.sd + bandwidth.sum.statsL$B1.spatial.var + 
  bandwidth.sum.statsL$B2.spatial.var + bandwidth.sum.statsL$sample.size:bandwidth.sum.statsL$error.sd)

# with interaction between sample.size and B1sv, 
# interaction significant but B1 and error.sd no longer so
lin1.sdi.ss.B1 = lm(bandwidth.sum.statsL$sd ~ bandwidth.sum.statsL$sample.size + bandwidth.sum.statsL$error.sd + 
  +   bandwidth.sum.statsL$B1.spatial.var + bandwidth.sum.statsL$B2.spatial.var + 
  bandwidth.sum.statsL$B1.spatial.var:bandwidth.sum.statsL$sample.size)
