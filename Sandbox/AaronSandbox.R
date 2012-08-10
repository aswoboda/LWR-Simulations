source("SpecificationSims/SimFunctions.R")


Data = DataGen(41,1,1,.3)

output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
# What output do we want from this function and in what form?
# Want information for cross validation scores. 
# Want to save the true betas and estimated betas. 

output[[1]]