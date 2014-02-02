source("MixedGWR/helperFunctionsNewSCV.R")
##### from SimScript.R
set.seed(123452489)
sampleSize <- c(80) 
B0.SpVar <- c(0) # , 2, 4
B1.SpVar <- c(0) #, 2, 4
B2.SpVar <- c(0) # , 2, 4
errorSD <- c(0.25) #, 1, 2
numRepeats <- 10

#dataGenParameters <- expand.grid(numRepeats = numRepeats, B0SpatialVar = B0.SpVar, B1SpatialVar = B1.SpVar, B2SpatialVar = B2.SpVar, errorSD = errors, sampleSizes = sampleSizes)
######
# here is where we normally call mcMultParams, instead let's run the code of the function

# numModels <- nrow(dataGenParameters) #each row is a set of data parameters

uberList <- list() #empty list for the results
uberListNum <- 1 #this will be used to put each repeat in a new spot in the list

#loops through all of the inputs
#for(model in 1:numModels){
model = 1
  
#   repNum <- dataGenParameters[model, "numRepeats"]
#   ss <- dataGenParameters[model, "sampleSizes"]
#   error <- dataGenParameters[model, "errorSD"]
#   B0.SpVar <- dataGenParameters[model, "B0SpatialVar"]
#   B1.SpVar <- dataGenParameters[model, "B1SpatialVar"]
#   B2.SpVar <- dataGenParameters[model, "B2SpatialVar"]
  
#     start <- Sys.time()
#     tempUberOutput <- lapply(1:repNum, uberFunction, ss, error, B0.SpVar, B1.SpVar, B2.SpVar) #runs gwr
#     uberList[[uberListNum]] <- tempUberOutput #puts it into the output
#     uberListNum <- uberListNum + 1 #so the next run goes into the next spot on the list
#     end <- Sys.time()
#     print(difftime(end,start, units = "m"))
#     print(paste0("On parameter set ", model, " of ", numModels, " total sets. Total remaining: ", numModels - model))
#     print(paste0("This models parameters were: SS: ", ss, ", error: ", error, ", B0, B1, B2 spatial variations of ", B0.SpVar,", ", B1.SpVar, ", and ", B2.SpVar))
#     
#     saveRDS(listToArray(uberList), file = "mcOutput.rds") #see above in MC part for reason to use saveRDS
   
#}

# Now we would normally run lapply(....)
# instead, let's grab the code for the uberFunction
#uberFunction <- function(repetition, sampleSize, errorSD, B0.SpVar, B1.SpVar, B2.SpVar){
  
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
#####  
models = models[c(1, 8),]
#####
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
  
  
#  temp = megaMaker(ks, models = models[c(1,8),], data = mydata) #to test the results #previously models[1:8,]
  #######
  ####### HERE is where we might change something to only run certain models 
  ####### (maybe models = models[c(1,8),]) to just run model 1 and model 8
# add code from megaMaker function
#megaMaker = function(bandwidths, models, data) { #this function is written to require model 1 to always be GGG, but that should be how we are running it
  
  data = mydata 
  bandwidths = ks
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
  temp = megaList ### this is the end of the megaMaker function code

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
  
rm(list = ls()[c(-48, -55, -33, -32, -49)])
  #pull out the results we care about
  uberResults <- resultsToKeep.gen(results, trueModelNumber, metrics, metricRanks) #metrics and metricRanks are separate to make the loop in the function simpler; see that code
  
#   #add the parameter values for this uber run
#   paramValues <- c(sampleSize, errorSD, trueModelNumber, B0.SpVar, B1.SpVar, B2.SpVar)
#   paramMat <- matrix(paramValues, nrow = nrow(uberResults), ncol = length(paramValues), byrow = T)
#   colnames(paramMat) <- c("Sample Size", "Error", "True Model", "B0 SpVar", "B1 SpVar", "B2 SpVar")
#   uberResults <- cbind(uberResults, paramMat) #adds sample size and error to each row
#   
   uberResults[8:14, 1] #and done


