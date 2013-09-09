testmat <- matrix(1:16, nrow = 4, ncol = 4)
testmat

testmat2 <- matrix(1:16, nrow = 4, ncol = 4)
cbind(testmat, testmat2)

lapplyUber <- list(x,x)
lapplyuber
length(lapplyuber)


#returns the index of the minimum.  You can find any value by replacing min with that value too.
which(min(testmat) == testmat, arr.ind = T)





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





# sampleSizes <- c(29, 30)
# B0.SpVar <- c(2)
# B1.SpVar <- c(1)
# B2.SpVar <- c(0)
# errors <- c(.5)
# numRepeats <- 1
# 
# dataGenParams <- expand.grid(numRepeats = numRepeats, sampleSizes = sampleSizes, B0SpatialVar = B0.SpVar, B1SpatialVar = B1.SpVar, B2SpatialVar = B2.SpVar, errorSD = errors)
# 


# uberListTest <- mcMultParams(dataGenParams)

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
  B0.SpVars <- unique(multParamRuns[,"B0 SpVar"])
  B1.SpVars <- unique(multParamRuns[,"B1 SpVar"])
  B2.SpVars <- unique(multParamRuns[,"B2 SpVar"])
  
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

mcMultParamsTimeTest <- function(dataGenParameters, MC = FALSE){ #runs data on a list of true models for a list of sample sizes and error terms
  
  numModels <- nrow(dataGenParameters) #each row is a set of data parameters
  
  colNames <- colnames(dataGenParameters)
  colNames[1] <- "Avg Time"
  timeTables <- matrix(NA, nrow = numModels, ncol = ncol(dataGenParameters))
  colnames(timeTables) <- colNames
  
  uberList <- list()
  uberListNum <- 1 #this will be used to put each repeat in a new spot in the list
  rowNum <- 1
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
      saveRDS(listToArray(uberList), file = "mcOutput.rds") #see above in MC part for reason to use saveRDS
      
      end <- Sys.time()
      print(difftime(end,start, units = "m"))
      print(paste0("On parameter set ", model, " of ", numModels, " total sets. Total remaining: ", numModels - model))
      print(paste0("This models parameters were: SS: ", ss, ", error: ", error, ", B0, B1, B2 spatial variations of ", B0.SpVar,", ", B1.SpVar, ", and ", B2.SpVar))
      
      timeTables[rowNum, ] <- c(difftime(end,start, units = "m")/repNum, ss, B0.SpVar, B1.SpVar, B2.SpVar, error)
      timeTables[rowNum,]
      rowNum <- rowNum + 1
      
    }
    
  }
  
  timeTables
}

mcRunTimeEst(sampleSizes, numRepeats){
  
}


sampleSizes <- c(100, 500, 700, 1000) #c(50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
B0.SpVar <- c(0, 2)
B1.SpVar <- c(0, 2)
B2.SpVar <- c(0, 2)
errors <- c( 1, 2)
numRepeats <- 5

testParams <- expand.grid(numRepeats = numRepeats, B0SpatialVar = B0.SpVar, B1SpatialVar = B1.SpVar, B2SpatialVar = B2.SpVar, errorSD = errors, sampleSizes = sampleSizes)

z <- mcMultParamsTimeTest(testParams, MC = F)

x <- mcMultParams(testParams, MC = T)


successRate.gen(x)
successRate.gen(x, successMeasure = "Betas", successRank = 10)
successRate.gen(x, successMeasure = "Both")


successRate.gen(a)
#successRate.gen(x, successMeasure = "Betas", successRank = 10)
#successRate.gen(x, successMeasure = "Both")
