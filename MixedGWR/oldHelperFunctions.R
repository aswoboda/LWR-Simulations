
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

DGP = function(n = 100) {
  # n = number of observations in our simulation
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
  mydata
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
