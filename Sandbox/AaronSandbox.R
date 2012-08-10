


source("SpecificationSims/SimFunctions.R")
Data = DataGen(41,1,1,.3)
output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)



betaComparer = function(i, betas) {
  sum((betas[, 1] - betas[, i])^2)
}
plot(cor(beta1hats)[1, -1], unlist(lapply(2:9, betaComparer, betas = beta1hats)))
