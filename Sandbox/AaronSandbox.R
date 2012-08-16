# empty sandbox


source("SpecificationSims/SimFunctions.R")

# set our simulation parameters
Replications = 500
sample.size = c(50, 100, 200, 500, 1000, 2000)
error.sd = c(3)
B1.spatial.var = 0.25
B2.spatial.var = 0.25

sim.parameters = expand.grid(sample.size, error.sd, B1.spatial.var, B2.spatial.var)
names(sim.parameters) = c("sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var")

meta.sim.num = dim(sim.parameters)[1]

DGPparameters = sim.parameters[1, ]
    


Data = DataGen(DGPparameters$sample.size, 
               DGPparameters$error.sd, 
               DGPparameters$B1.spatial.var, 
               DGPparameters$B2.spatial.var)
output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
new.output = Reorganizer(output)
simMetrics = LWRMetrics(new.output, Data)
optimal.bandwidths = bandwidth.Selector(simMetrics)

list(c(optimal.bandwidths, myfunction(Data)))

