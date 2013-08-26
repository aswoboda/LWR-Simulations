
##################### Over Head Functions ############################################
bisquare = function(d, dk) { # takes a vector of distances and a threshold distance
  weights = (1 - (d/dk)^2)^2 # for distances less than dk, weights are here
  weights[d>dk] = 0 # for distances farther away than dk, weights are zero
  weights 
}

GCV = function(y, yhat, levs, nonstationary = 0) {
  sample.size = length(y)
  v1 = nonstationary + colSums(matrix(levs))
  SE <- colSums((matrix(y) - matrix(yhat))^2)
  gcv <- sample.size*SE/(sample.size - v1)^2
  gcv
}

##  mixedLWR (generates fitted values for each observation after removing that observation as well) ##

mixedLWR = function(bandwidth, stationary = c("TRUE", "TRUE", "TRUE"), vars = c("x0", "x1", "x2"),  
                    locVars = c("east", "north"), depVar = "y", dataframe = mydata) {

  n = dim(dataframe)[1]
  totalVars = length(vars)
  statVars = vars[which(stationary == "TRUE")]
  nonstatVars = vars[which(stationary == "FALSE")]
  numStationary = length(statVars) # how many of them are there?
  numNonstationary = length(nonstatVars) # how many of them are there?
  coeffs = matrix(NA, n, totalVars)
  coeffsWithout = matrix(NA, n, totalVars) #this won't be in the output but is necessary to calculate the fitted values without
  colnames(coeffs) = vars
  colnames(coeffsWithout) = vars
  fittedValues = matrix(NA, n, 1)
  leverages = matrix(NA, n, 1)
  fittedValuesWithout = matrix(NA, n, 1)
  
  distMatrix = as.matrix(dist(dataframe[, locVars]))
  
  if (length(nonstatVars) == 0) { 
    modelType = "GGG"
    RHS = paste0(statVars, collapse="+")
    model2run = paste0(depVar, "~", RHS, "-1")
    lm.out = lm(model2run, data = dataframe)    
    for (i in 1:length(statVars)) coeffs[, i] = coef(lm.out)[i]
    fittedValues[, 1] = lm.out$fitted.values
    leverages[, 1] = lm.influence(lm.out, do.coef=FALSE)$hat
    
    for(obs in 1:n){ #to add the fitted values without
      dataframe$weightsWithout <- 1 #puts a vector of 1s
      dataframe$weightsWithout[obs] <- 0 #and removest he current observation
      lm.out.without = lm(model2run, data = dataframe)   
      fittedValuesWithout[obs, 1] <- lm.out.without$fitted.values[obs]
    }
  }
  
  if (length(statVars) == 0) { 
    modelType = "LLL"
    RHS = paste0(nonstatVars, collapse="+")
    model2run = paste0(depVar, "~", RHS, "-1")
    
    for (obs in 1:n) { # for each observation
      mydists = distMatrix[, obs] # grab the distances
      dk = sort(mydists)[bandwidth+1] # grab the distance to the kth obs
      dataframe$WEIGHTS = bisquare(mydists, dk) # calculate the weights
      
      lmreg = lm(model2run, data = dataframe, weights = WEIGHTS) # run the GWR
      
      fittedValues[obs, 1] = lmreg$fitted.values[obs] # grab the fitted y-value
      coeffs[obs, ] = coef(lmreg) # grab the fitted y-value
      leverages[obs, ] = lm.influence(lmreg, do.coef = FALSE)$hat[as.character(obs)] # grab the leverage value
      
      #get fitted values without current observation
      dataframe$WEIGHTS[obs] <- 0 #remove the current observation
      lmreg = lm(model2run, data = dataframe, weights = WEIGHTS) # run the GWR
      fittedValuesWithout[obs,] <- lmreg$fitted.values[obs]
    }
  }
  
  if (length(nonstatVars) > 0 & length(statVars) > 0) { 
    modelType <- ""
    for (var in stationary){
      if( var) {modelType <- paste0(modelType, "G")}
      else {modelType <- paste0(modelType, "L")}
    }
    
    
    LWRbetas = matrix(NA, n, numNonstationary) # creates a matrix of all the different coefficient estimates we'll need
    LWRbetasWithout <- matrix(NA, n, numNonstationary) # these won't be outputted, but are necessary to calculate the fitted values without
    
    step1 = matrix(NA, n, numStationary) # matrix for the step 1 results
    step1Without = matrix(NA, n, numStationary) #matrix for step 1 resutls without the current observation
    colnames(step1) = statVars
    colnames(step1Without) = statVars
    
    step3 = matrix(NA, nrow = n, ncol = 1) # matrix for the step 2 results
    step3Without = matrix(NA, nrow = n, ncol = 1) # matrix for the step 2 results without current obs
    
    for (obs in 1:n) { # now, for each observation in the dataset...
      mydists = distMatrix[, obs] # grab the distances between this observation and all others
      dk = sort(mydists)[bandwidth+1] # grab the distance to the kth nearest observation
      dataframe$WEIGHTS = bisquare(mydists, dk) # caculate the weights for all observations
      
      dataframe$weightsWithout <- dataframe$WEIGHTS #dupliate the weights
      dataframe$weightsWithout[obs] <- 0 #remove current observation
      
      for (xa in statVars) { # now for each variable we want to treat as stationary...
        RHS = paste0(nonstatVars, collapse = "+") # start the Right Hand Side of the regression equation
        model2run = paste0(xa, "~", RHS, "-1") # finish the model we'll run
        temp.lm = lm(model2run, data = dataframe, weights = WEIGHTS) # run the step 1 regression
        step1[obs, xa] = temp.lm$residuals[obs] # grab the residual for this observation
      
        temp.lm.without <- lm(model2run, data = dataframe, weights = weightsWithout)
        step1Without[obs, xa] <- temp.lm.without$residuals[obs]
      }
      
      
      step2model = paste0(depVar, "~", RHS, "-1") # create the model to run for step 2
      temp.lm = lm(step2model, data = dataframe, weights = WEIGHTS) # run the regression for step 2
      
      temp.lm.without <- lm(step2model, data = dataframe, weights = weightsWithout) #run regression for step 2 without current obs
      # step 3
      step3[obs, 1] = temp.lm$residuals[obs] # grab the residual from step 2 
      step3Without[obs, 1] = temp.lm.without$residuals[obs] #grab residual from step 2 without the current observation
    
    }
    
    # step 4: regress the step 2 residuals on the step 1 residuals
    lmOLS = lm(step3 ~ step1 - 1)
    lmOLSWithout = lm(step3Without ~ step1Without - 1)
    
    ahat = coef(lmOLS) # grab the estimated coefficients - these are our estimates of the stationary coefficients
    ahatWithout = coef(lmOLSWithout)
    
    names(ahat) = statVars
    names(ahatWithout) = statVars
    
    
    # step 5: subtract X*ahat from y and then use GWR (with this difference as the dependent value) 
    # to estimate the non-stationary coefficients
    dataframe$ytemp = dataframe[, depVar] - as.matrix(dataframe[, statVars])%*%ahat
    dataframe$ytempWithout = dataframe[, depVar] - as.matrix(dataframe[, statVars])%*%ahatWithout
    
    for (obs in 1:n) { # for each observation in our data set
      mydists = distMatrix[, obs] # grab the distances between it and all others
      dk = sort(mydists)[bandwidth+1] # grab the distance to the kth nearest observation
      dataframe$WEIGHTS = bisquare(mydists, dk) # calculate the weights
      dataframe$weightsWithout <- dataframe$WEIGHTS #duplicate weights
      dataframe$weightsWithout[obs] <- 0 #and remove the current observation
      
      LWRRHS = paste0(nonstatVars, collapse = "+") # start formulating the RHS of the LWR regression
      LWRmodel = paste0("ytemp~", LWRRHS, "-1") # finish the model to be estimated
      LWRmodelWithout <- paste0("ytempWithout~", LWRRHS, "-1") # finish the model to be estimated without
      
      lmreg = lm(LWRmodel, data = dataframe, weights = WEIGHTS) # run the regression
      lmregWithout = lm(LWRmodelWithout, data = dataframe, weights = weightsWithout) # run the regression without the current obs
      
      LWRbetas[obs, ] <- coef(lmreg) # keep track of the coefficient estimate
      LWRbetasWithout[obs, ] <- coef(lmregWithout) # get the betas without the current observations; don't need the leverages
      leverages[obs, ] <- lm.influence(lmreg, do.coef = FALSE)$hat[as.character(obs)] # keep track of the leverage values
    }
    
    # for each stationary variable, grab the value in ahat and place it in the appropriate columne for all observations
    for (i in 1:numStationary){
      coeffs[, statVars[i]]    = ahat[i] 
      coeffsWithout[, statVars[i]] <- ahatWithout[i]
    }
    # for each non-stationary variable, grab the vector of coefficients and stick it in the appropriate column
    for (i in 1:numNonstationary) {
      coeffs[, nonstatVars[i]] = LWRbetas[, i]
      coeffsWithout[, nonstatVars[i]] <- LWRbetasWithout[,i]
    }
    
    # now that we've got the coefficients, we can calculate the predicted y values 
    fittedValues = rowSums(coeffs*dataframe[, vars]) 
    fittedValuesWithout = rowSums(coeffsWithout*dataframe[,vars])
    fittedValues = matrix(fittedValues, n, 1)
    fittedValuesWithout <- matrix(fittedValuesWithout, n, 1)
  }
  
  out = list(ModelType = modelType, 
             Coefficients = coeffs,
             FittedValues = fittedValues,
             Leverages = leverages,
             FittedValuesWithout = fittedValuesWithout)
  out
}

### Mega Function (runs mixedLWR on a vector of bandwiths and models) ########

#this function is written to require model 1 to always be GGG, but that should be how we are running it
megaMaker = function(bandwidths, models, data) {
  
  mydata <- data
  megaList = list()
  
  #this is a list of NAs of the same size as all the other lists 
  #I think there should be an easier way to make this list, but I wasn't able to find one
  #the purpose is to make later loops simpler
  
  
  noResultsGGGInput <- list(ModelType = "GGG", 
                            Coefficients = matrix(NA, nrow = length(data$y), ncol = 3),
                            FittedValues = matrix(NA, nrow = length(data$y), ncol = 1),
                            Leverages = matrix(NA, nrow = length(data$y), ncol = 1),
                            FittedValuesWithout = matrix(NA, nrow = length(data$y), ncol = 1))
  megaList[[1]] <- list(noResultsGGGInput, noResultsGGGInput, noResultsGGGInput, 
                        noResultsGGGInput, noResultsGGGInput, noResultsGGGInput, 
                        mixedLWR(max(bandwidths), models[1,], data = data))
  
  #now the first item in megaList is a bunch of NAs, then model 1 at the appropriate bandwidth
  #without the NAs, for each metric we would need to do a special case for model 1 and input it directly
  #this should make those loops easier
  
  #now for models 2 through 8 ...
  for (i in 2:dim(models)[1]) {
    megaList[[i]] = lapply(bandwidths, mixedLWR, stationary = models[i, ], data = data)
  }
  
  
  ##these name generators could be replaced by just passing them as parameters to the function;
  ##they have to be generated for the results matrix anyway, but calcualting them in the function
  ##does let you run fewer than the max number of models and bandwidths
  
  #making the model names
  modelNames <- c()
  for(modelNum in 1:nrow(models)){
    modelType <- ""
    for (var in models[modelNum,]){
      if( var) {modelType <- paste0(modelType, "G")}
      else {modelType <- paste0(modelType, "L")}
    }
    modelNames <- c(modelNames, modelType)
  }
  
  #adding them to the eventual megaList output
  names(megaList) <- modelNames
  
  #bandwidth names
  bandwidthNames <- c()
  for(bwNum in 1:length(bandwidths)){
    bandwidthNames <- c(bandwidthNames, paste0("Bandwidth ", bwNum," of ", bandwidths[bwNum], " observations"))
  }
  
  #adding the bandwidths to each model
  for(modelNum in 1:nrow(models)){
    names(megaList[[modelNum]]) <- bandwidthNames
  }
  
  megaList
}

#### megaList manipulation functions (results creation functions) #####

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

###### Uber output generator (turns results (from megaMaker) into the final product) #####

resultsToKeep.gen <- function(results, trueModelNumber, metrics, metricRanks){
  uberOutput <- matrix(NA, nrow = 2*length(metrics), ncol = 2 + length(metrics) + length(metricRanks)) #generate the output
  colnames(uberOutput) <- c("Model Number", "Bandwidth", metrics, metricRanks) #puts the column names in place, the last are where the rankings for each metric will be stored
  rownames(uberOutput) <- c(paste0("True Model ", metrics), metrics)
  
  #input the true data
  for(metric in metrics){
    minMetricTrue <- min(results[,trueModelNumber, metric], na.rm = T) #find the smallest value of the metric for the true model
    minMetricTrueBW <- which(minMetricTrue == results, arr.ind = T)[1] #this picks out the bandwidth number
    uberOutput[paste0("True Model ", metric), "Model Number"] <- trueModelNumber #put true model into the output
    uberOutput[paste0("True Model ", metric), "Bandwidth"] <- minMetricTrueBW #and its bandwidth
    uberOutput[paste0("True Model ", metric), 3:ncol(uberOutput)] <- results[minMetricTrueBW, trueModelNumber, ] #and filling in every thing else
  } 
  
  #now the unrestricted minimization
  for(metric in metrics){
    minMetric <- min(results[,, metric], na.rm = T)
    minMetricBW <- which(minMetric == results, arr.ind = T)[1] #this picks out the bandwidth number
    minMetricModel <- which(minMetric == results, arr.ind = T)[2] #and the model number
    uberOutput[metric, "Model Number"] <- minMetricModel
    uberOutput[metric, "Bandwidth"] <- minMetricBW #this just returns the bandwidth number (1 through 7)
    uberOutput[metric, 3:ncol(uberOutput)] <- results[minMetricBW, minMetricModel, ]
  } 
  uberOutput
}


##### the uber function (generates data for megaMaker, runs everything) #########

uberFunction <- function(repetition, sampleSize, errorSD, trueModelNumber, B0.SpVar, B1.SpVar, B2.SpVar){
  
  start <- Sys.time() #to time the function
  
  #data gen
  n = sampleSize # number of observations in our simulation
  east = runif(n) # create a location variable
  north = runif(n) # create another location variable
  x0 = rep(1, n) # create a vector of 1's to serve as the intercept column
  x1 = runif(n) # create a vector for x1 values
  x2 = runif(n) # create a vector for x2 values
  error = rnorm(n, 0, errorSD) # create an error term
  
  ########################### Beta generators; this is the ineligant part, we have to enter each combination we want
  #global, global, global
  if(trueModelNumber == 1){
    B0 = 3 
    B1 = 2 
    B2 = 2 
    
  } else if(trueModelNumber ==2){
    
    #local, global, global
    
    B0 = 3*east + 3*north 
    B1 = 2 
    B2 = 2 
    
  } else if(trueModelNumber ==3){
    
    #global, local, global
    
    B0 = 3 
    B1 = 4*east - 2*north 
    B2 = 2 
    
  } else if(trueModelNumber ==4){
    
    #local, local, global
    
    B0 = 3*east  
    B1 = - 2*north
    B2 = 2 
    
  } else if(trueModelNumber ==5){
    
    #global, global, local
    B0 = 3 
    B1 = 2
    B2 = 4*east - 2*north
    
    
  } else if(trueModelNumber ==6){
    
    #local, global, local
    B0 = 3*east 
    B1 = 2 
    B2 = -2*north  
    
  } else if(trueModelNumber ==7){
    
    #global, local, local
    
    B0 = 3 
    B1 = - 2*north  
    B2 = 3*east
    
  } else if(trueModelNumber ==8){ #these ARE correlated 
    
    #local, local, local
    
    B0 <- 3*east + 3*north
    B1 <- 4*east - 2*north
    B2 <- -2*east + 4*north
    
  } else {
    
    print("Your model number is out of range")
  }
  
  
  
  y = B0*x0 + B1*x1 + B2*x2 + error # generate the dependent variable values according to our 
  mydata = data.frame(y, x0, x1, x2, east, north) # put everything together into a data frame
  
  ## these store the true values of the betas for use in the RMSE functions;
  #the way the function is implemented, it shouldnt matter if they are single values or vectors
  
  trueB0 <- B0
  trueB1 <- B1
  trueB2 <- B2
  # X0 = X1 = X2 = c("stationary", "non-stationary")
  # models = expand.grid(x0 = X0, x1 = X1, x2 = X2) # makes a data frame of all the different models we could run
  
  X0 = X1 = X2 = c("TRUE", "FALSE") #true is stationary
  models = as.matrix(expand.grid(x0 = X0, x1 = X1, x2 = X2))
  
  numk = 7 # number of bandwidths we'll use
  krat = 2/3 # rate at which bandwidths decrease (ie. 45 -> 30 -> 20, etc.)
  ks = n-1 # this is the largest value of k
  for (i in 2:numk) ks = c(round(krat*min(ks), 0), ks) # this generates the vector of bandwidths
  
  ################
  ##generate array to store results
  #First, I generate the names for all the variables 
  
  metrics <- c("AIC", "GCV", "SCV", "LOOCV", "B0RMSE", "B1RMSE", "B2RMSE")
  metricRanks <- c("AIC Rank", "GCV Rank","SCV Rank","LOOCV Rank", "B0RMSE Rank", "B1RMSE Rank", "B2RMSE Rank")
  
  #the model names are GGG, LGG, ...
  modelNames <- c()
  for(modelNum in 1:nrow(models)){
    modelType <- ""
    for (var in models[modelNum,]){
      if( var) {modelType <- paste0(modelType, "G")}
      else {modelType <- paste0(modelType, "L")}
    }
    modelNames <- c(modelNames, modelType)
  }
  
  #names for the bandwidths
  bandwidthNames <- c()
  for(bwNum in 1:length(ks)){
    bandwidthNames <- c(bandwidthNames, paste0("Bandwidth ", bwNum," of ", ks[bwNum], " observations"))
  }
  
  #and finally, the actual results array
  results <- array(NA, dim = c(length(ks), nrow(models), length(metrics) + length(metricRanks)), 
                   dimnames = list(bandwidthNames, modelNames, c(metrics, metricRanks)))
  
  
  temp = megaMaker(ks, models = models[1:8,], data = mydata) #to test the results
  
  
  
  ##input all the beta coefficients, these slowly fill in all the NAs
  results <- input.rmse(0, temp, trueB0, results)
  results <- input.rmse(1, temp, trueB1, results)
  results <- input.rmse(2, temp, trueB2, results)
  
  #metric inputs
  results <- input.gcv(mydata$y, temp, results)
  results <- input.aic(mydata$y, temp, results)
  results <- input.scv(mydata$y, temp, results)
  results <- input.loocv(mydata$y, temp, results)
  
  #generate rankings
  results <- rank.results(results, metrics)
  
  #pull out the results we care about
  uberResults <- resultsToKeep.gen(results, trueModelNumber, metrics, metricRanks) #metrics and metricRanks are separate to make the loop in the function simpler; see that code
  
  #add the parameter values for this uber run
  params <- matrix(c(sampleSize, errorSD, trueModelNumber), nrow = nrow(uberResults), ncol = 3, byrow = T)
  colnames(params) <- c("Sample Size", "Error", "True Model")
  uberResults <- cbind(uberResults, params) #adds sample size and error to each row
  
  end <- Sys.time()
  print(round(difftime(end, start, units = "m"), 2)) #prints out the time difference; could add more to print statement depdning on how we run repeated uber functions
  
  
  uberResults #and done
  
}


###### running uber ######

# x <- uberFunction(1, sampleSize = 29, 1.5, 3)
# x

#### combine results of an lapply ####

combine.list <- function(lapplyUber){ #this takes a list of uberFunction outputs (like from an lapply) and combines them all into one matrix
  finalOutput <- matrix(NA, ncol = ncol(lapplyUber[[1]])) #need a matrix with the right number of columns for adding rows 
  
  for(matrix in 1:length(lapplyUber)){ #go through all the outputs
    finalOutput <- rbind(finalOutput, lapplyUber[[matrix]]) #and stick them on the bottom of the output
  }
  finalOutput <- finalOutput[-1,] #removes the first row that was just NAs
  finalOutput
}

combine.list2 <- function(lapplyUber){ #does the same as the previous function, but for the second items in the list (for the )
  finalOutput <- matrix(NA, ncol = ncol(lapplyUber[[1]][[1]])) #need a matrix with the right number of columns for adding rows 
  
#  finalOutputArray <- array(c(length(lapplyUber), ))
  
  for(rep in 1:length(lapplyUber)){ #go through all the repetitions
    for(matrix in 1:length(lapplyUber[[1]])){
      finalOutput <- rbind(finalOutput, lapplyUber[[rep]][[matrix]]) #and stick them on the bottom of the output
    }
  }
  finalOutput <- finalOutput[-1,] #removes the first row that was just NAs
  finalOutput
}





#### running repetitions ####

### this is adapted from the SimFunctions.R file; the next function works better I think though
gwrSimulationReplicator = function(N = 2, sampleSize, errorSD, trueModelNumber, MC = FALSE){
  
  if(MC == TRUE) {
    require(multicore, quietly = TRUE)
    temp = mclapply(1:N, uberFunction, sampleSize, errorSD, trueModelNumber)
  }
  else temp = lapply(1:N, uberFunction, sampleSize, errorSD, trueModelNumber)
  temp
}
#x <-gwrSimulationReplicator(2, 29, .5, 2)
###



mcMultParamRuns <- function(numRepeats, sampleSizes, errors, trueModels, MC = FALSE){ #runs data on a list of true models for a list of sample sizes and error terms
  
  uberListNum <- 1 #this puts each repeat in a new spot in the list
  uberList <- list() #empty list for the results
    #loops through all of the inputs
    for(ss in sampleSizes){
      for(error in errors){
        for(trueModel in trueModels){
          if (MC){
            require(multicore, quietly = TRUE)
            tempUberOutput <- mclapply(1:numRepeats, uberFunction, ss, error, trueModelNumber = trueModel) #runs gwr
            uberList[[uberListNum]] <- tempUberOutput #puts it into the output
            uberListNum <- uberListNum + 1 #so the next run goes into the next spot on the list
          } else{
            tempUberOutput <- lapply(1:numRepeats, uberFunction, ss, error, trueModelNumber = trueModel) #runs gwr
            uberList[[uberListNum]] <- tempUberOutput #puts it into the output
            uberListNum <- uberListNum + 1 #so the next run goes into the next spot on the list
          }
        }
      }
    }
  uberListMatrix <- combine.list2(uberList) #convert everything into a matrix
  write.csv(uberListMatrix, file = paste0("Uber Output Large Test for Model ", trueModel, ".csv"))
  uberListMatrix
}

##### mc results interpretation #######


#this makes the output from the mclapply interpretable; the array returned gives the proportion of times each model metric selected the true model (or the one with the best beta coefficients)
#this NEEDS to have sample sizes strictly greater than 50 passed to it and NEEDS multiple runs in uberList passed to it (at least 2 of SS, errors, true models, or repeats).  If not, the indexing gets thrown off.  
#the options for successMeasure are "True Model", "Betas", or "Both" which are picking the true model, picking the lowest RMSE for the betas, and doing both at once
#success rank is the range the ranking of the beta RMSEs that are a success, meaning changing it to 2 calls having a beta RMSE rank of 1 or 2 a success
successRate.gen <- function(multParamRuns, successMeasure = "True Model", nrowPerRun = 14, successRank = 1){ #default success is picking the true model; nrowPerRun will change if we add another metric
  #these pull all the inputs into multParamRuns out
  errors <- unique(multParamRuns[,"Error"])
  sampleSizes <- unique(multParamRuns[,"Sample Size"])
  trueModels <- unique(multParamRuns[,"True Model"])
  metrics <- colnames(multParamRuns)[3:6] #as of 8/26, these have all the metric names (metric being JUST the CV/AIC methods)
  
  
  #Now I turn the inputted matrix into an array to make picking out the relevant results easier (used later, this requires the number of rows per run)
  rownames <- rownames(multParamRuns[1:nrowPerRun,])
  colnames <- colnames(multParamRuns)
  numRuns <- nrow(multParamRuns)/nrowPerRun
  multParamArray <- array(NA, c(length(rownames), length(colnames), numRuns), 
                          dimnames = list(rownames, colnames, c(paste0("Run ", 1:numRuns))))
  #this puts all the matrixes into the array where dim3 is the number of runs
  for(run in 1:numRuns){
    multParamArray[,,run] <- multParamRuns[((run - 1)*nrowPerRun + 1):(run*nrowPerRun),]
  }
  
  #below is the array that will eventaully be returned
  if(successMeasure == "True Model"){ #if picking the true model is the success measure, there is only one value per true model/metric, so this array is sufficient 
    successRate <- array(NA, c(length(metrics), length(trueModels), length(errors), length(sampleSizes)), 
                         dimnames = list(metrics, paste0("True Model ", trueModels), paste0("Error of ",errors), paste0("Sample Size of ", sampleSizes)))
  } 
  if(successMeasure == "Betas" | successMeasure == "Both"){ #if picking the best betas is the success measure, there are three values per true model/metric, so an extra dimension is necessary
    successRate <- array(NA, c(length(metrics), length(trueModels), 4, length(errors), length(sampleSizes)), 
                         dimnames = list(metrics, paste0("True Model ", trueModels), c(paste0("B", 0:2), "All Three"), paste0("Error of ",errors), paste0("Sample Size of ", sampleSizes)))   
  }
  
  for(error in errors){
    for(sampleSize in sampleSizes){
      for(trueModel in trueModels){
        #this is all we need to know to pick out the relevent runs for this combination of parameters, which is just the number of repeats from mcMultParamRun
        relevantRuns <- unique(which(multParamArray[,"Sample Size",] == sampleSize & multParamArray[,"Error",] == error & multParamArray[,"True Model",] == trueModel, arr.ind = T)[,2])
        for(metric in metrics){#now go through the metrics and fill in successRate
          if(successMeasure == "True Model"){ #default success measure, if minimizing the metric picks out the true model
            successProp <- sum(multParamArray[metric, "Model Number",relevantRuns] == trueModel)/length(relevantRuns) #proportion of times true model was sellected
            successRate[paste(metric), paste("True Model", trueModel), paste("Error of", error), paste("Sample Size of", sampleSize)] <- successProp
          }
          if(successMeasure == "Betas"){ #success proportions for picking the model + bandwidth with the lowest beta RMSE
            betaSuccesses <- matrix(NA, nrow = 3, ncol = length(relevantRuns))
            for(beta in 0:2){ 
              betaSuccesses[beta + 1, ] <- multParamArray[metric, paste0("B", beta, "RMSE Rank"),relevantRuns] <= successRank
              successProp <- sum(betaSuccesses[beta + 1, ])/length(relevantRuns) 
              successRate[paste(metric), paste("True Model", trueModel), paste0("B", beta), paste("Error of", error), paste("Sample Size of", sampleSize)] <- successProp
              } 
            #now for picking the minimum for all three at once, could be modified by chaning == 2 for getting excactly (or at least two)
            numSuccessPerRun <- colSums(betaSuccesses) #if the sum is 3, then all were a success on that run
            successProp <- sum(numSuccessPerRun == 3)/length(relevantRuns) #calculates that proportion
            successRate[paste(metric),  paste("True Model", trueModel), "All Three", paste("Error of", error), paste("Sample Size of", sampleSize)] <- successProp
          }
          if(successMeasure == "Both"){ #success measure is picking both true model and lowest RMSE
            betaSuccesses <- matrix(NA, nrow = 3, ncol = length(relevantRuns))
            for(beta in 0:2){ 
              betaSuccesses[beta + 1, ] <- multParamArray[metric, paste0("B", beta, "RMSE Rank"),relevantRuns] <= successRank
              modelSuccesses <- multParamArray[metric, "Model Number",relevantRuns] == trueModel
              numBothSucceed <- sum(betaSuccesses[beta + 1,] == modelSuccesses &  modelSuccesses == TRUE)
              successProp <- numBothSucceed/length(relevantRuns)
              successRate[paste(metric), paste("True Model", trueModel), paste0("B", beta), paste("Error of", error), paste("Sample Size of", sampleSize)] <- successProp
            } 
            #now for picking the minimum for all three at once, could be modified by chaning == 2 for getting excactly (or at least two)
            numSuccessPerRun <- colSums(betaSuccesses) #if the sum is 3, then all were a success on that run
            modelSuccesses <- multParamArray[metric, "Model Number",relevantRuns] == trueModel #runs where the true model was picked
            
            successProp <- sum(numSuccessPerRun == 3 & modelSuccesses == T)/length(relevantRuns) #calculates that proportion of runs where both events occured
            successRate[paste(metric),  paste("True Model", trueModel), "All Three", paste("Error of", error), paste("Sample Size of", sampleSize)] <- successProp
          }
        }
      }
    }
  }
  successRate
}

sampleSizes <- c(51)
trueModels <- 2
errors <- c(.005)

x <- mcMultParamRuns(2, sampleSizes, errors, trueModels)

successRate.gen(x)
successRate.gen(x, successMeasure = "Betas", successRank = 5)
successRate.gen(x, successMeasure = "Both")