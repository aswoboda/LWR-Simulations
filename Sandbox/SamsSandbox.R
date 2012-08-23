source("SpecificationSims/SimFunctions.R")

DGPparameters = data.frame(sample.size = 30, 
                            error.sd = 5,
                            B1.spatial.var = 1,
                            B2.spatial.var = .5)

Data = DataGen(DGPparameters$sample.size, 
               DGPparameters$error.sd, 
               DGPparameters$B1.spatial.var, 
               DGPparameters$B2.spatial.var)
output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
new.output = Reorganizer(output)
# simMetrics = LWRMetrics(new.output, Data)
LWRoutput = new.output


 R2.calc = function(LWRoutput, Data) {
   
  R2.output = 1 - (var(Data$dep.var - LWRoutput$yhats[,4])/var(Data$dep.var))
  diag(R2.output)
}