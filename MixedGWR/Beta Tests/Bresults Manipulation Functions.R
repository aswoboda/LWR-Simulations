#calculate room mean square error

rmse.calc <- function(estVal, trueVal){
  #mean square error
  mse <- sum((estVal - trueVal)^2)
  
  rmse <- (mse/length(estVal))^.5
  rmse
}

RMSE.calc.Bresults <- function(betaToTest, bandwidth, repeatNum, modelNum, sampleSize, Bresults){
  #determine the beta
  betaNum <- betaToTest[length(betaToTest)]
  trueBeta <- Bresults[paste0("Observation ", 1:sampleSize), paste0("True B", betaNum), bandwidth, repeatNum, modelNum, paste0("Sample Size of ", sampleSize)]
  estBeta <- Bresults[paste0("Observation ", 1:sampleSize), paste0("Estimated B", betaNum), bandwidth, repeatNum, modelNum, paste0("Sample Size of ", sampleSize)]
  
  rmse.calc(estBeta, trueBeta)
}

RMSEPercentile.calc.Bresults <- function(betaToTest, bandwidth, repeatNum, modelNum, sampleSize, Bresults, percentile){
  #determine the beta
  betaNum <- betaToTest[length(betaToTest)]
  trueBeta <- Bresults[paste0("Observation ", 1:sampleSize), paste0("True B", betaNum), bandwidth, repeatNum, modelNum, paste0("Sample Size of ", sampleSize)]
  estBeta <- Bresults[paste0("Observation ", 1:sampleSize), paste0("Estimated B", betaNum), bandwidth, repeatNum, modelNum, paste0("Sample Size of ", sampleSize)]
  
  errors <- sort(trueBeta - estBeta)
  maxErrorInd <- round(length(errors)*percentile, 0)
  
  rmse <- (sum(errors[1:maxErrorInd]^2, na.rm = T))^.5
  rmse
}

SDPercentile.calc.Bresults <- function(betaToTest, bandwidth, repeatNum, modelNum, sampleSize, Bresults, percentile){
  #determine the beta
  betaNum <- betaToTest[length(betaToTest)]
  trueBeta <- Bresults[paste0("Observation ", 1:sampleSize), paste0("True B", betaNum), bandwidth, repeatNum, modelNum, paste0("Sample Size of ", sampleSize)]
  estBeta <- Bresults[paste0("Observation ", 1:sampleSize), paste0("Estimated B", betaNum), bandwidth, repeatNum, modelNum, paste0("Sample Size of ", sampleSize)]
  
  errors <- sort(trueBeta - estBeta)
  maxErrorInd <- round(length(errors)*percentile, 0)
  
  sdErrors <- sd(errors[1:maxErrorInd], na.rm = T)
  sdErrors
}


BresultsMetrics.gen <- function(Bresults, sampleSizes){
  
  #replace observations
  newDim <- c(3,dim(Bresults)[-1])
  #replace the six T/E betas with one spot for each vector's RMSE
  newDim[2] <- 3
  
  
  newDimNames <- dimnames(Bresults)
  newDimNames[[1]] <- c("RMSE", "RMSE.90", "SD Error")
  newDimNames[[2]] <- c("B0", "B1", "B2")
  
  BresultsMetrics <- array(NA, dim = newDim, dimnames = newDimNames)
  
  #get the size of the dimenstions of Bresults for the loop
  resultsDim <- dim(Bresults)
  for(ss in 1:length(sampleSizes)){
    for(modelNum in 1:resultsDim[5]){
      for(repeatNum in 1:resultsDim[4]){
        for(bandwidthNum in 1:resultsDim[3]){
          for(betaNum in 0:2){
            
            BresultsMetrics[1, betaNum + 1, bandwidthNum, repeatNum, modelNum, ss] <- RMSEPercentile.calc.Bresults(betaNum, bandwidthNum, repeatNum, modelNum, sampleSizes[ss], Bresults, 1)
            BresultsMetrics[2, betaNum + 1, bandwidthNum, repeatNum, modelNum, ss] <- RMSEPercentile.calc.Bresults(betaNum, bandwidthNum, repeatNum, modelNum, sampleSizes[ss], Bresults, .9)
            BresultsMetrics[3, betaNum + 1, bandwidthNum, repeatNum, modelNum, ss] <- SDPercentile.calc.Bresults(betaNum, bandwidthNum, repeatNum, modelNum, sampleSizes[ss], Bresults, 1)
            
            
          }
        } 
      }
    }
  }
  BresultsMetrics
}

get.dimNames <- function(Ind, array){
  pointDimNames <- c()
  dimNames <- dimnames(array)
  
  for(dim in 1:length(Ind)){
    pointDimNames <- c(pointDimNames, dimNames[[dim]][Ind[dim]])
  }
  pointDimNames
}


