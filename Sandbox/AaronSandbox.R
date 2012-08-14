


source("SpecificationSims/SimFunctions.R")
Data = DataGen(41,1,1,.3)
output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
new.output = Reorganizer(output)

simMetrics = LWRMetrics(new.output)

