
### running multiple parameters
## This function runs multiple sets of parameters for sample size, error size, number of repetions, and spatial variation in B0, B1, and B2.  
## It needs to be passed a matrix where each row is a set of parametrers.  
## the columns of the matrix need to be named the same as they are called in lines 15 through 20, but the order does not matter
## For each repetition, it runs uberFunction (implemented later) which generates a data set and runs each of the 8 models and collects the GCV, LOOCV, SCV, AIC, B0RMSE, B1RMSE, B2RMSE of each model at 7 bandwidth levels

mcMultParams <- function(dataGenParameters, MC = FALSE){ #runs data on a list of true models for a list of sample sizes and error terms
  
  numModels <- nrow(dataGenParameters) #each row is a set of data parameters
  
  uberList <- list() #empty list for the results
  uberListNum <- 1 #this will be used to put each repeat in a new spot in the list
  
  #loops through all of the inputs
  for(model in 1:numModels){
    
    repNum <- dataGenParameters[model, "numRepeats"]
    ss <- dataGenParameters[model, "sampleSizes"]
    error <- dataGenParameters[model, "errorSD"]
    B0.SpVar <- dataGenParameters[model, "B0SpatialVar"]
    B1.SpVar <- dataGenParameters[model, "B1SpatialVar"]
    B2.SpVar <- dataGenParameters[model, "B2SpatialVar"]
    
    if (MC){
      require(multicore, quietly = TRUE)
      start <- Sys.time()
      tempUberOutput <- mclapply(1:repNum, uberFunction, ss, error, B0.SpVar, B1.SpVar, B2.SpVar) #runs gwr
      uberList[[uberListNum]] <- tempUberOutput #puts it into the output
      uberListNum <- uberListNum + 1 #so the next run goes into the next spot on the list
      end <- Sys.time()
      print(difftime(end,start, units = "m"))
      print(paste0("On parameter set ", model, " of ", numModels, " total sets. Total remaining: ", numModels - model))
      print(paste0("This models parameters were: SS: ", ss, ", error: ", error, ", B0, B1, B2 spatial variations of ", B0.SpVar,", ", B1.SpVar, ", and ", B2.SpVar))
      
      saveRDS(listToArray(uberList), file = "mcOutput.rds") #saveRDS lets you rename an object when reloading it into the environment, x <- readRDS("mcOutput.rds") assigns x as the name for the R data
      
    } else{
      start <- Sys.time()
      tempUberOutput <- lapply(1:repNum, uberFunction, ss, error, B0.SpVar, B1.SpVar, B2.SpVar) #runs gwr
      uberList[[uberListNum]] <- tempUberOutput #puts it into the output
      uberListNum <- uberListNum + 1 #so the next run goes into the next spot on the list
      end <- Sys.time()
      print(difftime(end,start, units = "m"))
      print(paste0("On parameter set ", model, " of ", numModels, " total sets. Total remaining: ", numModels - model))
      print(paste0("This models parameters were: SS: ", ss, ", error: ", error, ", B0, B1, B2 spatial variations of ", B0.SpVar,", ", B1.SpVar, ", and ", B2.SpVar))
      
      saveRDS(listToArray(uberList), file = "mcOutput.rds") #see above in MC part for reason to use saveRDS
    }
    
  }
  
  uberListArray <- listToArray(uberList) #convert everything into an array
  saveRDS(uberListArray, file = "mcOutputFinal.rds") #see above in MC part for reason to use saveRDS
  uberListArray
}

####first, the listToArray function used above.  
## This function converts the list output from an mclapply or lapply of the kind generated in the above function into an array 
## where the third dimension is the total number of runs of uberfunction (accross all models). 


listToArray <- function(list){ #takes a list of the kind outputted by the internals of mcMultParams and makes it into an array
  numModels <- length(list)
  numRepeats <- c()
  #get the numer of repeats for each model below; this allows for some models to be replicated more than others (this may be helpful for running smaller samples sizes more times than larger ones)
  for(model in 1:numModels){
    numRepeats <- c(numRepeats, length(list[[model]]))
  }
  
  totalRepeats <- sum(numRepeats) #gets the total number of repeptions of all the models for the dimension of the array
  numRows <- nrow(list[[1]][[1]])
  numCols <- ncol(list[[1]][[1]])
  rowNames <- rownames(list[[1]][[1]])
  colNames <- colnames(list[[1]][[1]])
  
  #now I generate the final array
  finalArray <- array(NA, dim = c(numRows, numCols, totalRepeats), dimnames = list(rowNames, colNames, paste0("Repetition ", 1:totalRepeats)))
  
  #now I loop through each model and the number of repetitions of that model and put the results into the array
  repArrayNum <- 1 #this keeps track of which repetion for the array we are on
  for(model in 1:numModels){
    for(rep in 1:numRepeats[model]){
      finalArray[,,repArrayNum] <- list[[model]][[rep]] #and the matrix is inputted into the array
      repArrayNum <- repArrayNum + 1
    }
  }
  
  finalArray
}

#this output is much easier to work with than a list.  

#### Now, the uberFunction that was called.  This one is MUCH longer. 
## This takes a set of parameters (sample size, error, and beta spatial variation) and generates a data set, then tests each GWR model (GGG, LGG, ...) and 
## outputs a matrix of the true model's metric scores + ranks (AIC, GCV, B0 RMSE, etc.) and the model selected by minimizing each of those criteria

uberFunction <- function(repetition, sampleSize, errorSD, B0.SpVar, B1.SpVar, B2.SpVar){
  
  #calculate true model number
  modelStat <- c(NA, NA, NA)
  if(B0.SpVar == 0) {modelStat[1] <- TRUE
  }else modelStat[1] <- FALSE
  
  if(B1.SpVar == 0) {modelStat[2] <- TRUE
  } else modelStat[2] <- FALSE
  
  if(B2.SpVar == 0) {modelStat[3] <- TRUE
  } else modelStat[3] <- FALSE
  
  #generate the possible models ...
  X0 = X1 = X2 = c("TRUE", "FALSE") #true is stationary
  models = as.matrix(expand.grid(x0 = X0, x1 = X1, x2 = X2))
  
  #and find the true model number
  trueModelNumber <- which(modelStat[1] == models[,1] & modelStat[2] == models[,2] & modelStat[3] == models[,3]) #returns the row number where all the variables match
  
  #data gen
  n = sampleSize # number of observations in our simulation
  east = runif(n) # create a location variable
  north = runif(n) # create another location variable
  x0 = rep(1, n) # create a vector of 1's to serve as the intercept column
  x1 = runif(n) # create a vector for x1 values
  x2 = runif(n) # create a vector for x2 values
  error = rnorm(n, 0, errorSD) # create an error term
  
  
  #now generating the betas...
  #they will ALWAYS have an expectation of 2 and have an expected correlation of 0 (except for corner cases in LLL)  
  B1 <- 2 - .5*B1.SpVar + east*B1.SpVar
  B2 <- 2 - .5*B2.SpVar + north*B2.SpVar
  B0 <- 2 + B0.SpVar*(.387452 + cos(2*pi*sqrt((east - .5)^2 + (north- .5)^2)/sqrt(.5)))
  
  
  #finally the y values
  y = B0*x0 + B1*x1 + B2*x2 + error # generate the dependent variable values according to our 
  mydata = data.frame(y, x0, x1, x2, east, north) # put everything together into a data frame
  
  ## these store the true values of the betas for use in the RMSE functions;
  #the way the function is implemented, it shouldnt matter if they are single values or vectors
  
  trueB0 <- B0
  trueB1 <- B1
  trueB2 <- B2
  # X0 = X1 = X2 = c("stationary", "non-stationary")
  # models = expand.grid(x0 = X0, x1 = X1, x2 = X2) # makes a data frame of all the different models we could run
  
  
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
  paramValues <- c(sampleSize, errorSD, trueModelNumber, B0.SpVar, B1.SpVar, B2.SpVar)
  paramMat <- matrix(paramValues, nrow = nrow(uberResults), ncol = length(paramValues), byrow = T)
  colnames(paramMat) <- c("Sample Size", "Error", "True Model", "B0 SpVar", "B1 SpVar", "B2 SpVar")
  uberResults <- cbind(uberResults, paramMat) #adds sample size and error to each row
  
  uberResults #and done
  
}

#the output of this is stored in a list generated in mcMultParams


# First, I document the megaMaker function called, then the ones used to generate the results.  
#The megaMaker recieves the data generated in uberFunction as its input, then it assumes each of the GWR models in turn and returns the fitted values, coefficient estimates, leverages, and fitted values without each observation.
# for each model and bandwidth combination.  That data is too cumbersome to use, so the results functions slowly build a more workable output for uberFunction to return.  

#now for megaMaker

megaMaker = function(bandwidths, models, data) { #this function is written to require model 1 to always be GGG, but that should be how we are running it
  
  mydata <- data
  megaList = list()
  
  #below is a list of NAs of the same size as all the other lists. Model 1 is only run on the max bandwidth, so we need to input NAs into the list for those runs
  noResultsGGGInput <- list(ModelType = "GGG", 
                            Coefficients = matrix(NA, nrow = length(data$y), ncol = 3),
                            FittedValues = matrix(NA, nrow = length(data$y), ncol = 1),
                            Leverages = matrix(NA, nrow = length(data$y), ncol = 1),
                            FittedValuesWithout = matrix(NA, nrow = length(data$y), ncol = 1))
  
  #and inputing those NAs plus running gwr for the max bandwidth
  megaList[[1]] <- list(noResultsGGGInput, noResultsGGGInput, noResultsGGGInput, 
                        noResultsGGGInput, noResultsGGGInput, noResultsGGGInput, 
                        mixedLWR(max(bandwidths), models[1,], data = data))
  
  #now for models 2 through 8 ...
  for (i in 2:dim(models)[1]) {
    megaList[[i]] = lapply(bandwidths, mixedLWR, stationary = models[i, ], data = data)
  }
  
  
  
  
  #This generates a vector that contains the names for all the models run (GGG, LGG, ...)
  modelNames <- c()
  for(modelNum in 1:nrow(models)){
    modelType <- ""
    for (var in models[modelNum,]){ #this puts a G if the variable in the model is stationary (global) and an L if it is non-stationary (local)
      if( var) {modelType <- paste0(modelType, "G")}
      else {modelType <- paste0(modelType, "L")}
    }
    modelNames <- c(modelNames, modelType) #and this adds the name to the list
  }
  
  #adding them to the eventual megaList output
  names(megaList) <- modelNames
  
  #bandwidth names
  bandwidthNames <- c()
  for(bwNum in 1:length(bandwidths)){
    bandwidthNames <- c(bandwidthNames, paste0("Bandwidth ", bwNum," of ", bandwidths[bwNum], " observations"))
  }
  
  #adding the bandwidth names to each model
  for(modelNum in 1:nrow(models)){
    names(megaList[[modelNum]]) <- bandwidthNames
  }
  
  #and returning the list
  megaList
}

#this output is stored in the uberfuntion, but only part of it is returned (that will be described when I get to the result generating functions)

## megaMaker called a function mixedLWR, which I will document now.  It takes a single bandwidth, a single model, and a dataframe and runs the lwr regression.  
## It returns a list of fitted values, coefficient estimates, leverages, and fitted values without each observation

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

#this output is one item in the list returned by megaMaker

#the above function also calls bisquare to calculate weights, which is this function:
bisquare = function(d, dk) { # takes a vector of distances and a threshold distance
  weights = (1 - (d/dk)^2)^2 # for distances less than dk, weights are here
  weights[d>dk] = 0 # for distances farther away than dk, weights are zero
  weights 
}

### results generating functions (from within uberFunction)
##everything up until now has been about running the LWR, the following functions are about making the data readable.  megaMaker returns a list of everything, these slim that down.  
## for the next eight functions, the first four all calculate a model metric (GCV, SCV, ... ).  The next four take an empty array and the data needed to calculate the value for that metric and put it into the array.  

#first, the calcuation functions


calc.rmse <- function(trueVal, estVal){
  rmse <- (sum((trueVal - estVal)^2)/length(estVal))^.5 #calculates RMSE, works even one or both are vectors
  rmse
}

calc.gcv = function(y, yhat, levs, nonstationary = 0) {
  sample.size = length(y)
  v1 = nonstationary + colSums(matrix(levs))
  SE <- colSums((matrix(y) - matrix(yhat))^2)
  gcv <- sample.size*SE/(sample.size - v1)^2
  gcv
}

calc.aic <- function(y, yhat, lev, nonStationary){
  n <- length(y)
  v1 = nonStationary + sum(lev)
  errorSD <- apply(y - yhat, 2, sd)
  aic <- 2*n*log(errorSD) + n*log(2*pi) + n*(n + v1)/(n-2-v1)
  aic
}


calc.scv <- function(dep.var, yhats.without, na.rm = FALSE) {
  numer = ((dep.var - yhats.without)^2)
  denom = rowSums(numer, na.rm = na.rm) 
  stan.CV.values = colSums(numer/denom, na.rm = na.rm)
  stan.CV.values
}

calc.loocv <- function(y, yhatsWithout){
  sum((y - yhatsWithout)^2)
}

#the next four functions take the matrix they are putting the results in as an input.  That array is a k x 8 x m array where n is the number of bandwidths and m is the number of metrics
#for each metric, the value in the k x 8 matrix is the metric's value for that bandwidth (the row number) and that model (the column number).  
#the functions also take as inputs anything else needed to calcuate the metric that is not included in megaList (the output from the megaMaker function)

#for a given beta (just a number, i.e. to input B0 RMSE do betaToEval = 0 not betaToEval = "B0")
input.rmse <- function(betaToEval, megaList, trueBetaVal, results){ #this asks for the results array to input the results, so we don't need more code to do that
  modelNames <- names(megaList) #get names of models run
  bandwidthNames <- names(megaList[[2]]) #get bandwidth names
  
  #the following loops through the names of each model and bandwidth
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
      gcv <- calc.gcv(y, yhat, levs, numNonstationary)
      #and put it into the results matrix.  Again, this is done by model/BW name not number for if only a subset of models are run
      results[bandwidth, model, "GCV"] <- gcv #by indexing to "GCV" we can add metrics fairly easily, this will still put this result in the right place
    }
  }
  
  #returns the modified results input
  results
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

input.scv <- function(y, megaList, results){
  modelNames <- names(megaList) #get names of models run
  numModels <- length(modelNames)
  numBandwidths <- length(megaList[[2]]) #megaList[[1]] may only have 1 bandwidth for the GGG model, as it is now model 1 should have all bandwidths though
  n <- length(y)
  #this loops through the names of each model and bandwidth
  #if models are done in an unusual order this should still input the correct results
  
  #for the old SCV calculation, we standardized using a matrix where the rows are observations and columns were bandwidths (using a new matrix for each model)
  #but now, we want to standardize accross all models. Instead of making 8 individual matricies for each model, I make one large matrix with all of them. 
  fittedValuesWithoutMat <- matrix(NA, n, numBandwidths*numModels) #this first column will be deleted later
  colNum <- 1
  for(model in modelNames){ 
    for(bandwidth in 1:numBandwidths){
      #this collects the estimated coefficient, the indexing is a bit intense but this should get the coefficient estimates for the correct model and bandwidth
      yhatWithout <- megaList[[model]][[bandwidth]][[5]] #estimated ys without obs
      fittedValuesWithoutMat[,colNum] <- yhatWithout
      colNum <- colNum + 1
    }
  }
  
  #calc scv
  scvScores <- calc.scv(y, fittedValuesWithoutMat, na.rm = TRUE)
  scvScores[1:(numBandwidths - 1)] <- NA #these should be NAs as the GGG model doesn't run on non-global bandwidths
  
  for(scvScoreNum in 0:(length(scvScores) - 1)){
    modelNum <- (scvScoreNum %/% numBandwidths) + 1
    bandwidthNum <- (scvScoreNum %% numBandwidths) + 1
    results[bandwidthNum, modelNum, "SCV"] <- scvScores[scvScoreNum + 1]
  }

#returns the modified results input
results
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

## in this results array, we also want to keep track of the rank of each bandwith and model combination.  
## The following function fills in another m levels to the metric dimesnion of the array with the ranks of each combination for each metric (with 1 being the smallest value of the metric)

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

## This is the final output for the results array.  However, this is still probably too cumbersom for R to generate multiple versions of in a large mclapply.  
## The following function (also used in uberFunction) takes these results and generates a 2m x 2m + 2 matrix.  
## The first m rows' values are generated by searching within the TRUE model for the minimum of a given metric (determined by the row number).  The values for the row are then what model number is sellected, at what bandwidth, and what the values and ranks of all the other metrics for that combination are.  
## The next m rows' values are generated by searching accross ALL models for the minimum of a given metric (determined by the row number).  The values for the row are then what model number is sellected, at what bandwidth, and what the values and ranks of all the other metrics for that combination are.  
## This is that function.  


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

#the output of this function is most of the final output of the uberFunction.  Within the uberFunction, 6 more columns are added that are the same for every row.  
#those columns contain the parameter values for that run of the uberFunction, namely sample size, error, true model number, and the beta spatial variations.  


