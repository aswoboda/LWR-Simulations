
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



input.gcv(y, temp, results)

