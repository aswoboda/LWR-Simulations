

####
#### now let's use these functions to generate some data, run all the different possible GWR models on it, and see how their results compare
####

#vector of the smaple sizes to use
sampleSizes <- c(100, 300, 500, 1000)

numRepeats = 5 # the number of times we'll run this simulation

#for reference, here are the models
X0 = X1 = X2 = c("stationary", "non-stationary")
models = expand.grid(x0 = X0, x1 = X1, x2 = X2) # makes a data frame of all the different models we could run
rownames(models) = paste0("Model #", rownames(models))
modelMat = as.matrix(models) # we'll want a matrix version of the models
myvars = colnames(modelMat) # list of the variables we have


#parameters: sample sizes, true model, 

#CHANGE WHEN RUNNING NEW MODELS
trueModelNumber <- 4

results <- matrix(data = NA, nrow = length(sampleSizes)*numRepeats, ncol = 15, 
                  dimnames = list(1:(length(sampleSizes)*numRepeats), c("True Model Number", "True Model Variables", "Model Error", "Sample Size", "Repetition", 
                                                                        "True Model Min GCV", "True Model Bandwidth", 
                                                                        "Best Model Number", "Best Model Variables", 
                                                                        "Best Model GCV", "Best Model Bandwidth", "Best Model = True Model",
                                                                        "Second Best Model", "Second Best Model GCV", "Second Best Model Bandwidth")) 
)



trialNumber <- 0
for(trueModelNumber in 1:8) {
  for(sampleSize in sampleSizes){
    
    #generate name for the true model
    
    #global, global, global
    if(trueModelNumber == 1){
      trueModelName <- "GGG"
    }  else if(trueModelNumber ==2){
      
      #local, global, global
      
      trueModelName <- "LGG"
    } else if(trueModelNumber ==3){
      
      #global, local, global
      
      trueModelName <- "GLG"
    } else if(trueModelNumber ==4){
      
      #local, local, global
      
      trueModelName <- "LLG"
    } else if(trueModelNumber ==5){
      
      #global, global, local
      
      
      trueModelName <- "GGL"
    } else if(trueModelNumber ==6){
      
      #local, global, local
      
      trueModelName <- "LGL"
    } else if(trueModelNumber ==7){
      
      #global, local, local
      
      trueModelName <- "GLL"
    } else if(trueModelNumber ==8){   
      
      #local, local, local
      
      trueModelName <- "LLL"
    } else {
      
      print("Your model number is out of range")
    }
    
    #used in naming the pdf
    pdf(paste0("mixedGWRplots", sampleSize, "-",trueModelName, ".pdf"), height = 8, width = 10)
    
    
    for (repeats in 1:numRepeats) { 
      trialNumber <- trialNumber + 1
      start <- Sys.time()
      ####
      #### Here's the Data Generation Process
      ####
      n = sampleSize #= 100 # number of observations in our simulation
      east = runif(n) # create a location variable
      north = runif(n) # create another location variable
      x0 = rep(1, n) # create a vector of 1's to serve as the intercept column
      x1 = runif(n) # create a vector for x1 values
      x2 = runif(n) # create a vector for x2 values
      
      
      
      #global, global, global
      if(trueModelNumber == 1){
        B0 = 3 # we're going to say that the intercept is not function of location
        B1 = 2 # as is this one
        B2 = 2 # and this one
        
      } else if(trueModelNumber ==2){
        
        #local, global, global
        
        B0 = 3*east + 3*north # we're going to say that the intercept is a function of location
        B1 = 2 # but this one is not
        B2 = 2 # nor is this coefficient
        
      } else if(trueModelNumber ==3){
        
        #global, local, global
        
        B0 = 3 # we're going to say that the intercept is not a function of location
        B1 = 4*east - 2*north # but this one is 
        B2 = 2 # and this one is stationary
        
      } else if(trueModelNumber ==4){
        
        #local, local, global, #8/13, changed to make B1(E) and B0(N)
        
        B0 = 3*east + 3*north # we're going to say that the intercept is a function of location
        B1 = 4*east - 2*north # as is this one 
        B2 = 2 # but this one isn't
        
      } else if(trueModelNumber ==5){
        
        #global, global, local
        
        
      } else if(trueModelNumber ==6){
        
        #local, global, local
        B0 = 3*east + 3*north # we're going to say that the intercept is a function of location
        B1 = 2 # as is this one 
        B2 = 4*east - 2*north  # but this one isn't
        
      } else if(trueModelNumber ==7){
        
        #global, local, local
        
        B0 = 3 # we're going to say that the intercept is not a function of location
        B1 = 4*east - 2*north  # but this one is
        B2 = -2*east + 4*north # as is this one
        
      } else if(trueModelNumber ==8){
        
        #local, local, local
        
        B0 <- 3*east + 3*north
        B1 <- 4*east - 2*north
        B2 <- -2*east + 4*north
        
      } else {
        
        print("Your model number is out of range")
      }
      
      yTrue = B0*x0 + B1*x1 + B2*x2 
      errorSD <- .5*mean(yTrue)
      
      error = rnorm(n, 0, errorSD) # create an error term
      
      y = B0*x0 + B1*x1 + B2*x2 + error # generate the dependent variable values according to our 
      mydata = data.frame(y, x0, x1, x2, east, north) # put everything together into a data frame
      
      # now we calculate some distances that we'll need later on using the "fields" package
      dists = nearest.dist(mydata[, c("east", "north")], delta = 2) # i don't like this distance matrix calc... alternative?
      dists = t(dists) + dists # have to do this to make the ful distance matrix
      ###Graham: doesn't this double the distances?  It doesn't affect the ranking but it would affect the weights.  
      
      
      #####
      ##### now I want to run LWR on all 8 combinations of models to see which model gives the best fit
      #####
      
      
      numk = 7 # number of bandwidths we'll use
      krat = 2/3 # rate at which bandwidths decrease (ie. 45 -> 30 -> 20, etc.)
      ks = n-1 # this is the largest value of k
      for (i in 2:numk) ks = c(round(krat*min(ks), 0), ks) # this generates the vector of bandwidths
      
      GCVmat = matrix(NA, numk, dim(models)[1]+1) # making a matrix that we will fill in with our model results
      # a row for each bandwidth we will try, a column for all our models, and one additional column to hold the bandwidths
      GCVmat[, dim(models)[1]+1] = ks # put the ks in the final column
      
      # First model is all stationary
      lmOLS = lm(y ~ x0 + x1 + x2 - 1, data = mydata) # run OLS
      levOLS = lm.influence(lmOLS, do.coef = FALSE)$hat # calculate the leverages
      GCVmat[numk, 1] = GCV(mydata$y, lmOLS$fitted.values, levOLS) # calculate the GCV score for this model
      #print(GCVmat)
      
      for (modelnum in 2:7) { # these are the models that include a combination of stationary and non-stationary coefficients
        mix = modelMat[modelnum, ] # grab the row of values from the model matrix
        varsStationary = myvars[which(mix == "stationary")] # which variables are stationary?
        numStationary = length(varsStationary) # how many of them are there?
        varsNonstationary = myvars[which(mix == "non-stationary")] # which variables are non-stationary?
        numNonstationary = length(varsNonstationary) # how many of them are there?
        
        for (j in 1:numk) { # this loop goes through all of the different bandwidth values
          k =ks[j] # set the bandwidth for this iteration
          LWRbetas = matrix(NA, n, numNonstationary) # creates a matrix of all the different coefficient estimates we'll need
          #LWRyhat = matrix(NA, n, 1) # and a place to hold the predicted values
          LWRlevs = matrix(NA, n, 1) # and the leverage values
          
          step1 = matrix(NA, nrow = n, ncol = numStationary) # matrix for the step 1 results
          colnames(step1) = varsStationary
          
          step3 = matrix(NA, nrow = n, ncol = 1) # matrix for the step 2 results
          
          for (i in 1:n) { # now, for each observation in the dataset...
            mydists = dists[, i] # grab the distances between this observation and all others
            dk = sort(mydists)[k+1] # grab the distance to the kth nearest observation
            myweights = bisquare(mydists, dk) # caculate the weights for all observations
            
            for (xa in varsStationary) { # now for each variable we want to treat as stationary...
              RHS = paste0(varsNonstationary, collapse = "+") # start the Right Hand Side of the regression equation
              model2run = paste0(xa, "~", RHS, "-1") # finish the model we'll run
              temp.lm = lm(model2run, data = mydata, weights = myweights) # run the step 1 regression
              step1[i, xa] = temp.lm$residuals[i] # grab the residual for this observation
            }
            
            step2model = paste0("y~", RHS, "-1") # create the model to run for step 2
            temp.lm = lm(step2model, data = mydata, weights = myweights) # run the regression for step 2
            
            # step 3
            step3[i, 1] = temp.lm$residuals[i] # grab the residual from step 2 
          }
          
          # step 4: regress the step 2 residuals on the step 1 residuals
          lmOLS = lm(step3 ~ step1 - 1)
          ahat = coef(lmOLS) # grab the estimated coefficients - these are our estimates of the stationary coefficients
          names(ahat) = varsStationary
          
          # step 5: subtract X*ahat from y and then use GWR (with this difference as the dependent value) 
          # to estimate the non-stationary coefficients
          ytemp = mydata$y - as.matrix(mydata[, varsStationary])%*%ahat
          
          for (i in 1:n) { # for each observation in our data set
            mydists = dists[, i] # grab the distances between it and all others
            dk = sort(mydists)[k+1] # grab the distance to the kth nearest observation
            myweights = bisquare(mydists, dk) # calculate the weights
            
            LWRRHS = paste0(varsNonstationary, collapse = "+") # start formulating the RHS of the LWR regression
            LWRmodel = paste0("ytemp~", LWRRHS, "-1") # finish the model to be estimated
            lmreg = lm(LWRmodel, data = mydata, weights = myweights) # run the regression
            LWRbetas[i, ] <- coef(lmreg) # keep track of the coefficient estimate
            #LWRyhat[i] = lmreg$fitted.values[i] # keep track of the fitted values from this regression
            LWRlevs[i] <- lm.influence(lmreg, do.coef = FALSE)$hat[as.character(i)] # keep track of the leverage values
          }
          
          betas = matrix(NA, n, 3) # this matrix will store all our coefficients for each observation
          colnames(betas) = myvars
          
          # for each stationary variable, grab the value in ahat and place it in the appropriate columne for all observations
          for (i in 1:numStationary)    betas[, varsStationary[i]]    = ahat[i] 
          # for each non-stationary variable, grab the vector of coefficients and stick it in the appropriate column
          for (i in 1:numNonstationary) betas[, varsNonstationary[i]] = LWRbetas[, i]
          
          # now that we've got the coefficients, we can calculate the predicted y values 
          yhats = rowSums(betas*mydata[, myvars]) 
          # and use the true y-values, the predicted y-values, and the leverages, to calculate the GCV score
          GCVmat[j, modelnum] = GCV(mydata$y, yhats, LWRlevs, nonstationary = numNonstationary)
        } 
      }
      
      # Last model is all non-stationary (GWR on everything)
      LWRyhat = matrix(NA, n, 1) # matrix to hold the predicted y-values
      LWRlevs = matrix(NA, n, 1) # matrix to hold the leverages
      
      for (j in 1:numk) { # for each k value
        k =ks[j] # set the k
        
        for (i in 1:n) { # now for each observation
          mydists = dists[, i] # grab the distances
          dk = sort(mydists)[k+1] # grab the distance to the kth obs
          weights = bisquare(mydists, dk) # calculate the weights
          
          lmreg = lm(y ~ x0 + x1 + x2 - 1, data = mydata, weights = weights) # run the GWR
          LWRyhat[i] = lmreg$fitted.values[i] # grab the fitted y-value
          LWRlevs[i] <- lm.influence(lmreg, do.coef = FALSE)$hat[as.character(i)] # grab the leverage value
        }
        
        GCVmat[j, 8] = GCV(mydata$y, LWRyhat, LWRlevs) # now calculate the GCV score
      }
      print(round(GCVmat, 3)) # print up the GCV matrix if needed for future reference
      matplot(GCVmat[, 9], GCVmat[, -9], type = "o", # this makes the plot that goes in the .pdf file
              xlab = "number of observations in regression",
              ylab = "GCV score",
              main = paste0("Sample Size for ", trueModelName, " of ", sampleSize, " observations"))
      
      
      
      ###entering data into results
      #generate row number
      currentSampleNumber <- which(sampleSize == sampleSizes) #gives index for how many sample sizes have been completed previously
      currentRepetition <- repeats
      
      resultsNumber <- trialNumber #was indexed to add one for each loop
      
      #input generics
      results[resultsNumber, "True Model Number"] <- trueModelNumber
      results[resultsNumber, "Sample Size"] <- sampleSize
      results[resultsNumber, "Repetition"] <- repeats
      results[resultsNumber, "True Model Variables"] <- paste0(models[trueModelNumber, 1], ", ", models[trueModelNumber, 2], ", ", models[trueModelNumber, 3])
      results[resultsNumber, "Model Error"] <- round(errorSD, 2)
      
      ##collect true model data
      #min GCV in true model's column
      trueModelMinGCV <- min(GCVmat[,trueModelNumber], na.rm = TRUE)
      
      trueModelIndex <- which(trueModelMinGCV == GCVmat[,trueModelNumber], arr.ind = T) #returns the index of the location of the true model's min GCV
      trueModelBandwidthVal <- GCVmat[trueModelIndex[1],9] #returns the bandwidth value by finding the correct row and looking in the 9th column
      
      #enter true model data
      
      results[resultsNumber, "True Model Min GCV"] <- trueModelMinGCV
      results[resultsNumber, "True Model Bandwidth"] <- trueModelBandwidthVal
      
      
      ##collect best model data
      minGCV <- min(GCVmat[,1:8], na.rm = TRUE) #finds minimum over all models, ignoring NAs
      #if the true model is the best, nothing more needs to be calculated
      
      
      #collect best model data
      
      
      bestModelIndex <- which(minGCV == GCVmat, arr.ind = T)
      bestModelNum <- bestModelIndex[2] #columns correspond to model numbers
      bestModelBandwidthVal <- GCVmat[bestModelIndex[1], 9]
      
      results[resultsNumber, "Best Model Number"] <- bestModelNum
      results[resultsNumber, "Best Model Variables"] <- paste0(models[bestModelNum, 1], ", ", models[bestModelNum, 2], ", ", models[bestModelNum, 3])
      results[resultsNumber, "Best Model GCV"] <- minGCV
      results[resultsNumber, "Best Model Bandwidth"] <- bestModelBandwidthVal
      
      
      #determine if hte best model is the true model
      if(results[resultsNumber, "True Model Number"] == results[resultsNumber, "Best Model Number"]){
        results[resultsNumber, "Best Model = True Model"] <- T
      } else {
        results[resultsNumber, "Best Model = True Model"] <- F
      }
      
      
      
      #second best model data
      
      #ranks GCV scores and picks out second lowest
      rankedGCV <- sort(GCVmat[,1:8])    
      secondLowestGCV <- rankedGCV[2]
      
      results[resultsNumber, "Second Best Model GCV"] <- secondLowestGCV
      
      #identify the model
      secondBestIndex <- which(secondLowestGCV == GCVmat[,1:8], arr.ind = T)
      #input model and bandwidth
      results[resultsNumber, "Second Best Model"] <- secondBestIndex[2]
      results[resultsNumber, "Second Best Model Bandwidth"] <- GCVmat[secondBestIndex[1], 9]
      
      
      print(paste0("Simulation sample size = ", sampleSize, ", finished repetition ", repeats, " of ", numRepeats))
      print(Sys.time()) # this prints up what time it is after the iteration to keep track of longer running simulations
      end <- Sys.time()
      print(paste0("This repetition of ", sampleSize, " observations on model ", trueModelNumber, " with error of ", round(errorSD, 2), " took ", end - start, "min."))
    } #end of one repeat
    dev.off() # close the .pdf file 
  } #end of one sample size
  
} #end of model loop

write.csv(results, file = paste0("mixedGWRResultsAll8.csv")) #write csv with the results

print(models) # print out the models (helps when looking at the pdf figures)

