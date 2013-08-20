
setwd("~/Desktop") #sets working directory (and the location of pdfs created by this script)
require(fields) # make sure you have this package installed, otherwise the distance calcs won't work
# Mixed GWR as desribed on pp 67-8 in the GWR book by Fotheringham, et al.

# n observations
# a variables believed to have stationary effect over space in a matrix Xa
# b variables belived to have non-stationary effect over space in a matrix Xb

# for each column of Xa:
  # regress the column against Xb using basic GWR
  # compute the residuals from the above regression
# regress y against Xb using basic GWR
# compute the residuals from the above regression
# regress the y residuals against the Xa residuals using OLS -> this yields ahat, the stationary coefficients
# subtract Xa*ahat from y. regress this against Xb using basic GWR to obtain the geographically varying coefficients

# note that if there are ka a-group variables, then using this algorithm requires running basic GWR ka + 2 times

####
#### Some functions
####
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

####
#### now let's use these functions to generate some data, run all the different possible GWR models on it, and see how their results compare
####
pdf("mixedGWRplots.pdf", height = 8, width = 10)
numRepeats = 10 # the number of times we'll run this simulation
for (repeats in 1:numRepeats) { 
  ####
  #### Here's the Data Generation Process
  ####
  n = 100 # number of observations in our simulation
  east = runif(n) # create a location variable
  north = runif(n) # create another location variable
  x0 = rep(1, n) # create a vector of 1's to serve as the intercept column
  x1 = runif(n) # create a vector for x1 values
  x2 = runif(n) # create a vector for x2 values
  error = rnorm(n, 0, .5) # create an error term
  
  B0 = 2*east + north # we're going to say that the intercept is a function of location
  B1 = 1 # but this one is not
  B2 = 2 # nor is this coefficient
  y = B0*x0 + B1*x1 + B2*x2 + error # generate the dependent variable values according to our 
  mydata = data.frame(y, x0, x1, x2, east, north) # put everything together into a data frame
  
  # now we calculate some distances that we'll need later on using the "fields" package
  dists = nearest.dist(mydata[, c("east", "north")], delta = 2) # i don't like this distance matrix calc... alternative?
  dists = t(dists) + dists # have to do this to make the ful distance matrix
  
  #####
  ##### now I want to run LWR on all 8 combinations of models to see which model gives the best fit
  #####
  
  X0 = X1 = X2 = c("stationary", "non-stationary")
  models = expand.grid(x0 = X0, x1 = X1, x2 = X2) # makes a data frame of all the different models we could run
  rownames(models) = paste0("Model #", rownames(models))
  modelMat = as.matrix(models) # we'll want a matrix version of the models
  myvars = colnames(modelMat) # list of the variables we have
  
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
          main = "Visual Comparison of Model Performance")
  print(Sys.time()) # this prints up what time it is after the iteration to keep track of longer running simulations
}
dev.off() # close the .pdf file
print(models) # print out the models (helps when looking at the pdf figures)





### GOAL: write a mixedGWR function that we can use in our simulations

# inputs:
#########
# point at which to conduct mixed GWR? I don't think we can do this, as the algorithm requires multiple GWRs
# dependent variable
# stationary variables
# non-stationary variables
# location variables
# data.frame where we can find these variables
# bandwidth

# outputs:
##########
# coefficients
# standard errors?
# fitted.values
# leverage values

# Relies on
bisquare = function(d, dk) { # takes a vector of distances and a threshold distance
  weights = (1 - (d/dk)^2)^2 # for distances less than dk, weights are here
  weights[d>dk] = 0 # for distances farther away than dk, weights are zero
  weights 
}

mixedLWR = function(bandwidth, stationary = c("TRUE", "TRUE", "TRUE"), vars = c("x0", "x1", "x2"),  
                    locVars = c("east", "north"), depVar = "y", dataframe = mydata) {
  
  n = dim(dataframe)[1]
  totalVars = length(vars)
  statVars = vars[which(stationary == "TRUE")]
  nonstatVars = vars[which(stationary == "FALSE")]
  numStationary = length(statVars) # how many of them are there?
  numNonstationary = length(nonstatVars) # how many of them are there?
  coeffs = matrix(NA, n, totalVars)
  colnames(coeffs) = vars
  fittedValues = matrix(NA, n, 1)
  leverages = matrix(NA, n, 1)
  
  distMatrix = as.matrix(dist(dataframe[, locVars]))
  
  if (length(nonstatVars) == 0) { 
    modelType = "OLS"
    RHS = paste0(statVars, collapse="+")
    model2run = paste0(depVar, "~", RHS, "-1")
    lm.out = lm(model2run, data = dataframe)    
    for (i in 1:length(statVars)) coeffs[, i] = coef(lm.out)[i]
    fittedValues[, 1] = lm.out$fitted.values
    leverages[, 1] = lm.influence(lm.out, do.coef=FALSE)$hat
  }
  
  if (length(statVars) == 0) { 
    modelType = "LWR"
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
    }
  }
  
  if (length(nonstatVars) > 0 & length(statVars) > 0) { 
    modelType = "mixedLWR"
    
    LWRbetas = matrix(NA, n, numNonstationary) # creates a matrix of all the different coefficient estimates we'll need
    
    step1 = matrix(NA, n, numStationary) # matrix for the step 1 results
    colnames(step1) = statVars
    
    step3 = matrix(NA, nrow = n, ncol = 1) # matrix for the step 2 results
    
    for (obs in 1:n) { # now, for each observation in the dataset...
      mydists = distMatrix[, obs] # grab the distances between this observation and all others
      dk = sort(mydists)[bandwidth+1] # grab the distance to the kth nearest observation
      dataframe$WEIGHTS = bisquare(mydists, dk) # caculate the weights for all observations
      
      for (xa in statVars) { # now for each variable we want to treat as stationary...
        RHS = paste0(nonstatVars, collapse = "+") # start the Right Hand Side of the regression equation
        model2run = paste0(xa, "~", RHS, "-1") # finish the model we'll run
        temp.lm = lm(model2run, data = dataframe, weights = WEIGHTS) # run the step 1 regression
        step1[obs, xa] = temp.lm$residuals[obs] # grab the residual for this observation
      }
      
      step2model = paste0(depVar, "~", RHS, "-1") # create the model to run for step 2
      temp.lm = lm(step2model, data = dataframe, weights = WEIGHTS) # run the regression for step 2
      
      # step 3
      step3[obs, 1] = temp.lm$residuals[obs] # grab the residual from step 2 
    }
    
    # step 4: regress the step 2 residuals on the step 1 residuals
    lmOLS = lm(step3 ~ step1 - 1)
    ahat = coef(lmOLS) # grab the estimated coefficients - these are our estimates of the stationary coefficients
    names(ahat) = statVars
    
    # step 5: subtract X*ahat from y and then use GWR (with this difference as the dependent value) 
    # to estimate the non-stationary coefficients
    dataframe$ytemp = dataframe[, depVar] - as.matrix(dataframe[, statVars])%*%ahat
    
    for (obs in 1:n) { # for each observation in our data set
      mydists = distMatrix[, obs] # grab the distances between it and all others
      dk = sort(mydists)[bandwidth+1] # grab the distance to the kth nearest observation
      dataframe$WEIGHTS = bisquare(mydists, dk) # calculate the weights
      
      LWRRHS = paste0(nonstatVars, collapse = "+") # start formulating the RHS of the LWR regression
      LWRmodel = paste0("ytemp~", LWRRHS, "-1") # finish the model to be estimated
      lmreg = lm(LWRmodel, data = dataframe, weights = WEIGHTS) # run the regression
      LWRbetas[obs, ] <- coef(lmreg) # keep track of the coefficient estimate
      leverages[obs, ] <- lm.influence(lmreg, do.coef = FALSE)$hat[as.character(obs)] # keep track of the leverage values
    }
    
    # for each stationary variable, grab the value in ahat and place it in the appropriate columne for all observations
    for (i in 1:numStationary)    coeffs[, statVars[i]]    = ahat[i] 
    # for each non-stationary variable, grab the vector of coefficients and stick it in the appropriate column
    for (i in 1:numNonstationary) coeffs[, nonstatVars[i]] = LWRbetas[, i]
    
    # now that we've got the coefficients, we can calculate the predicted y values 
    fittedValues = rowSums(coeffs*dataframe[, vars]) 
    fittedValues = matrix(fittedValues, n, 1)
  }
  
  out = list(ModelType = modelType, 
             Coefficients = coeffs,
             FittedValues = fittedValues,
             Leverages = leverages)
  out
}

output = mixedLWR(bandwidth = 80, stationary = c(FALSE, FALSE, TRUE))

temp = lapply(c(20, 40, 60, 80), mixedLWR, stationary = c(FALSE, FALSE, TRUE))

X0 = X1 = X2 = c("TRUE", "FALSE")
models = as.matrix(expand.grid(x0 = X0, x1 = X1, x2 = X2))

megaMaker = function(bandwidths, models, data) {
  megaList = list()
  for (i in 1:dim(models)[1]) {
    test = sum(models[i, ] == "FALSE")
    if(test == 0) {
      megaList[[i]] = lapply(max(bandwidths), mixedLWR, stationary = models[i, ])
      names(megaList[[i]]) = paste0("bw", max(bandwidths))
    } else  {
      megaList[[i]] = lapply(bandwidths, mixedLWR, stationary = models[i, ])
      names(megaList[[i]]) = paste0("bw", bandwidths)
    }
  }
  names(megaList) = paste0("model", 1:dim(models)[1])
  megaList
}

temp = megaMaker(c(20, 50, 80), models = models, data = mydata)