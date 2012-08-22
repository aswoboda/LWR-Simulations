source("SpecificationSims/SimFunctions.R")

DGPparameters = data.frame(sample.size = 30, 
                            error.sd = .5,
                            B1.spatial.var = 1,
                            B2.spatial.var = .5)

Data = DataGen(DGPparameters$sample.size, 
               DGPparameters$error.sd, 
               DGPparameters$B1.spatial.var, 
               DGPparameters$B2.spatial.var)
output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
new.output = Reorganizer(output)
simMetrics = LWRMetrics(new.output, Data)



 AIC.calc = function(new.output, Data) {
  sample.size = dim(new.output$leverages)[1]
  v1 = colSums(new.output$leverages)
  est.error.sd = apply(Data$dep.var - new.output$yhats, 2, sd)
  
  AICc.output = 2*sample.size*log(est.error.sd) + sample.size*log(2*pi) + 
    sample.size*((sample.size + v1)/(sample.size - 2 - v1))
  
  AICc.output
}
