
#betas
#given two inputs, this calculates RMSE.  
calc.rmse <- function(trueVal, estVal){
  rmse <- (sum((trueVal - estVal)^2)/length(estVal))^.5 #calculates RMSE, works even one or both are vectors
  rmse
}

#for a given beta (just a number, i.e. to input B0 RMSE do betaToEval = 0 not betaToEval = "B0")
input.rmse <- function(betaToEval, megaList, trueBetaVal, results){ #this asks for the results array to input the results, so we don't need more code to do that
  modelNames <- names(megaList) #get names of models run
  bandwidthNames <- names(megaList[[2]]) #megaList[[1]] may only have 1 bandwidth for the GGG model, as it is now model 1 should have all bandwidths though
  
  #this loops through the names of each model and bandwidth
  #if models are done in an unusual order this should still input the correct results
  for(model in modelNames){
    for(bandwidth in 1:length(bandwidthNames)){
      #this collects the estimated coefficient, the indexing is a bit intense but this should get the coefficient estimates for the correct model and bandwidth
      estCoeff <- megaList[[model]][[bandwidth]][[2]][,(betaToEval + 1)] #R indexes start at 1 so to get B0 you need to add 1
      
      #if the names of the dimensions of megaList are not GGG, LGG, ... use the next two lines to get the model name, this will store it in the results section properly too
      modelName <- megaList[[model]][[bandwidth]][[1]]
      model <- modelName
      
      #calc rmse
      rmse <- calc.rmse(trueBetaVal, estCoeff)
      #and put it into the results matrix.  Again, this is done by model/BW name not number for if only a subset of models are run
      results[bandwidth, model, paste0("B", betaToEval, "RMSE")] <- rmse #by indexing to "B0RMSE" we can add metrics fairly easily, this will still put this result in the right place
    }
  }
  
  #returns the modified results input
  results
}

#GCV

input.gcv <- function(y, megaList, results){
  modelNames <- names(megaList) #get names of models run
  numBandwidths <- length(megaList[[2]]) #megaList[[1]] may only have 1 bandwidth for the GGG model, as it is now model 1 should have all bandwidths though

  #this loops through the names of each model and bandwidth
  #if models are done in an unusual order this should still input the correct results
  for(model in modelNames){
    for(bandwidth in 1:numBandwidths){
      #this collects the estimated coefficient, the indexing is a bit intense but this should get the coefficient estimates for the correct model and bandwidth
      yhat <- megaList[[model]][[bandwidth]][[3]] #estimated ys
      levs <- megaList[[model]][[bandwidth]][[4]] #leverages
      
      
      #if the names of the dimensions of megaList are not GGG, LGG, ... use the next two lines to get the model name, this will store it in the results section properly too
      #modelName <- megaList[[model]][[bandwidth]][[1]]
      #model <- modelName
      
      #now to extract the number of non-stationary variables
      splitModel <- strsplit(model, "") #this splits the model into a list of length 3, so it needs to be indexed to 1 in the next step 
      numNonstationary <- sum(splitModel[[1]] == "L")     
      
      #calc gcv
      gcv <- GCV(y, yhat, levs, numNonstationary)
      #and put it into the results matrix.  Again, this is done by model/BW name not number for if only a subset of models are run
      results[bandwidth, model, "GCV"] <- gcv #by indexing to "GCV" we can add metrics fairly easily, this will still put this result in the right place
    }
  }
  
  #returns the modified results input
  results
}

#AIC

calc.aic <- function(y, yhat, lev, nonStationary){
  n <- length(y)
  v1 = nonStationary + sum(lev)
  errorSD <- apply(y - yhat, 2, sd)
  aic <- 2*n*log(errorSD) + n*log(2*pi) + n*(n + v1)/(n-2-v1)
  aic
}

input.aic <- function(y, megaList, results){
  modelNames <- names(megaList) #get names of models run
  numBandwidths <- length(megaList[[2]]) #megaList[[1]] may only have 1 bandwidth for the GGG model, as it is now model 1 should have all bandwidths though
  
  #this loops through the names of each model and bandwidth
  #if models are done in an unusual order this should still input the correct results
  for(model in modelNames){
    for(bandwidth in 1:numBandwidths){
      #this collects the estimated coefficient, the indexing is a bit intense but this should get the coefficient estimates for the correct model and bandwidth
      yhat <- megaList[[model]][[bandwidth]][[3]] #estimated ys
      levs <- megaList[[model]][[bandwidth]][[4]] #leverages
      
      
      #if the names of the dimensions of megaList are not GGG, LGG, ... use the next two lines to get the model name, this will store it in the results section properly too
      #modelName <- megaList[[model]][[bandwidth]][[1]]
      #model <- modelName
      
      #now to extract the number of non-stationary variables
      splitModel <- strsplit(model, "") #this splits the model into a list of length 3, so it needs to be indexed to 1 in the next step 
      numNonstationary <- sum(splitModel[[1]] == "L")     
      
      #calc gcv
      aic <- calc.aic(y, yhat, levs, numNonstationary)
      #and put it into the results matrix.  Again, this is done by model/BW name not number for if only a subset of models are run
      results[bandwidth, model, "AIC"] <- aic #by indexing to "AIC" we can add metrics fairly easily, this will still put this result in the right place
    }
  }
  
  #returns the modified results input
  results
}

#SCV

calc.scv <- function(dep.var, yhats.without) {
  numer = ((dep.var - yhats.without)^2)
  denom = rowSums(numer) 
  stan.CV.values = colSums(numer/denom)
  stan.CV.values
}

input.scv <- function(y, megaList, results){
  modelNames <- names(megaList) #get names of models run
  numBandwidths <- length(megaList[[2]]) #megaList[[1]] may only have 1 bandwidth for the GGG model, as it is now model 1 should have all bandwidths though
  n <- length(y)
  #this loops through the names of each model and bandwidth
  #if models are done in an unusual order this should still input the correct results
  
  #first, we need to loop through everything and extract the fitted values without 
  for(model in modelNames[-1]){ #this ignores the first model, which I assusme is GGG
    fittedValuesWithoutMat <- matrix(NA, n, numBandwidths) #this matrix should make calculating the SCV easier; it wil be filled with the fitted values without the current observation (the row number)
    for(bandwidth in 1:numBandwidths){
      #this collects the estimated coefficient, the indexing is a bit intense but this should get the coefficient estimates for the correct model and bandwidth
      yhatWithout <- megaList[[model]][[bandwidth]][[5]] #estimated ys without obs
      fittedValuesWithoutMat[,bandwidth] <- yhatWithout #putting them in the column
    }
    
      #if the names of the dimensions of megaList are not GGG, LGG, ... use the next two lines to get the model name, this will store it in the results section properly too
      #modelName <- megaList[[model]][[bandwidth]][[1]]
      #model <- modelName
            
      #calc scv
      scv <- calc.scv(y, fittedValuesWithoutMat)
      #and put it into the results matrix.  Again, this is done by model/BW name not number for if only a subset of models are run
      results[, model, "SCV"] <- scv #by indexing to "scv" we can add metrics fairly easily, this will still put this result in the right place
    
  }
  
  #returns the modified results input
  results
}

calc.loocv <- function(y, yhatsWithout){
  sum((y - yhatsWithout)^2)
}

input.loocv <- function(y, megaList, results){
  modelNames <- names(megaList) #get names of models run
  numBandwidths <- length(megaList[[2]]) #megaList[[1]] may only have 1 bandwidth for the GGG model, as it is now model 1 should have all bandwidths though
  n <- length(y)
  #this loops through the names of each model and bandwidth
  #if models are done in an unusual order this should still input the correct results  
  
  #first, we need to loop through everything and extract the fitted values without 
  for(model in modelNames){ 
    for(bandwidth in 1:numBandwidths){
      #this collects the estimated coefficient, the indexing is a bit intense but this should get the coefficient estimates for the correct model and bandwidth
      yhatWithout <- megaList[[model]][[bandwidth]][[5]] #estimated ys without obs
      loocv <- calc.loocv(y, yhatWithout)
    
    
      #if the names of the dimensions of megaList are not GGG, LGG, ... use the next two lines to get the model name, this will store it in the results section properly too
      #modelName <- megaList[[model]][[bandwidth]][[1]]
      #model <- modelName
      
      #calc scv
      #and put it into the results matrix.  Again, this is done by model/BW name not number for if only a subset of models are run
      results[bandwidth, model, "LOOCV"] <- loocv #by indexing to "loocv" we can add metrics fairly easily, this will still put this result in the right place
      
    }
  }
  #returns the modified results input
  results
}

#adding ranks, the metrics are ranked accross ALL models and bandwidths from lowest to highest
#so if bandwidth 7, model 2, GCV Rank = 8, then model 2 using bandwidth 7 has the 8th lowest GCV score of all models and bandwidths
rank.results <- function(results, metrics){
  for(metric in metrics){
    metricVals <- results[,,metric] #pulls out the metrics
    sortedMetrics <- sort(metricVals, na.last = T) #sort ranks the metrics from smallest to largest and returns them as a vector
    metricRank <- results[,,metric] #this coppies the NA locations which are dropped in the sort
    for(rank in 1:length(sortedMetrics)){
      index <- which(metricVals == sortedMetrics[rank]) #pulls out the cell number in metricVals that is the rank lowest
      metricRank[index] <- rank #replaces that cell with its appropriate ranking
    }
    results[,,paste0(metric, " Rank")] <- metricRank #puts the new matrix into results
  }
  results #and spits out the output
}

resultsToKeep.gen <- function(results, trueModelNumber, metrics, metricRanks){
  uberOutput <- matrix(NA, nrow = 12, ncol = 2 + length(metrics) + length(metricRanks)) #generate the output
  colnames(uberOutput) <- c("Model Number", "Bandwidth", metrics, metricRanks) #puts the column names in place, the last are where the rankings for each metric will be stored
  rownames(uberOutput) <- c("True Model AIC", "True Model GCV", "True Model SCV", "True Model B0RMSE", "True Model B1RMSE", "True Model B2RMSE", "AIC", "GCV", "SCV", "B0RMSE", "B1RMSE", "B2RMSE")
  
  #input the true data
  for(metric in metrics){
    minMetricTrue <- min(results[,trueModelNumber, metric], na.rm = T) #find the smallest value of the metric for the true model
    minMetricTrueBW <- which(minMetricTrue == results, arr.ind = T)[1] #this picks out the bandwidth number
    uberOutput[paste0("True Model ", metric), "Model Number"] <- trueModelNumber #put true model into the output
    uberOutput[paste0("True Model ", metric), "Bandwidth"] <- minMetricTrueBW #and its bandwidth
    uberOutput[paste0("True Model ", metric), 3:14] <- results[minMetricTrueBW, trueModelNumber, ] #and filling in every thing else
  } 
  
  #now the unrestricted minimization
  for(metric in metrics){
    minMetric <- min(results[,, metric], na.rm = T)
    minMetricBW <- which(minMetric == results, arr.ind = T)[1] #this picks out the bandwidth number
    minMetricModel <- which(minMetric == results, arr.ind = T)[2] #and the model number
    uberOutput[metric, "Model Number"] <- minMetricModel
    uberOutput[metric, "Bandwidth"] <- minMetricBW #this just returns the bandwidth number (1 through 7)
    uberOutput[metric, 3:14] <- results[minMetricBW, minMetricModel, ]
  } 
  uberOutput
}