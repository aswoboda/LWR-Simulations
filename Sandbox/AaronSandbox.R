source("SpecificationSims/SimFunctions.R")


Data = DataGen(41,1,1,.3)

output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
# What output do we want from this function and in what form?
# Want information for cross validation scores. 
# Want to save the true betas and estimated betas. 

temp = sapply(output, "[", 3) # grabbing the estimated dependent variable values
temp1 = unlist(temp)
temp2 = matrix(temp1, 41, 8, byrow = T)
yhats = cbind(Data$dep.var, temp2)
cor(yhats)[1, -1]

temp = sapply(output, "[", 1) # grabbing the estimated beta values
temp1 = unlist(temp)
beta0hats = matrix(temp1[seq(1, 41*3*8, 3)], 41, 8, byrow = T)
beta0hats = cbind(Data$trueB0, beta0hats)


beta1hats = matrix(temp1[seq(2, 41*3*8, 3)], 41, 8, byrow = T)
beta1hats = cbind(Data$trueB1, beta1hats)

beta2hats = matrix(temp1[seq(3, 41*3*8, 3)], 41, 8, byrow = T)
beta2hats = cbind(Data$trueB2, beta2hats)


betaComparer = function(i, betas) {
  sum((betas[, 1] - betas[, i])^2)
}
plot(cor(beta1hats)[1, -1], unlist(lapply(2:9, betaComparer, betas = beta1hats)))
