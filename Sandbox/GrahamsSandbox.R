testmat <- matrix(1:16, nrow = 4, ncol = 4)
testmat

#returns the index of the minimum.  You can find any value by replacing min with that value too.
which(min(testmat) == testmat, arr.ind = T)

#################################################################
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

#data gen
n = 50 # number of observations in our simulation
east = runif(n) # create a location variable
north = runif(n) # create another location variable
x0 = rep(1, n) # create a vector of 1's to serve as the intercept column
x1 = runif(n) # create a vector for x1 values
x2 = runif(n) # create a vector for x2 values
error = rnorm(n, 0, 1.5) # create an error term

B0 = 2*east + north # we're going to say that the intercept is a function of location
B1 = 1 # but this one is not
B2 = 2 # nor is this coefficient
y = B0*x0 + B1*x1 + B2*x2 + error # generate the dependent variable values according to our 
mydata = data.frame(y, x0, x1, x2, east, north) # put everything together into a data frame

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
#generate array to store results
metrics <- c("AIC", "GCV", "SCV", "B0RMSE", "B1RMSE", "B2RMSE")

modelNames <- c()
for(modelNum in 1:nrow(models)){
  modelType <- ""
  for (var in models[modelNum,]){
    if( var) {modelType <- paste0(modelType, "G")}
    else {modelType <- paste0(modelType, "L")}
  }
  modelNames <- c(modelNames, modelType)
}

bandwidthNames <- c()
for(bwNum in 1:length(ks)){
  bandwidthNames <- c(bandwidthNames, paste0("Bandwidth ", bwNum," of ", ks[bwNum], " observations"))
}


results <- array(NA, dim = c(length(ks), nrow(models), length(metrics)), 
                 dimnames = list(bandwidthNames, modelNames, metrics))


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
    modelType = "GGG"
    RHS = paste0(statVars, collapse="+")
    model2run = paste0(depVar, "~", RHS, "-1")
    lm.out = lm(model2run, data = dataframe)    
    for (i in 1:length(statVars)) coeffs[, i] = coef(lm.out)[i]
    fittedValues[, 1] = lm.out$fitted.values
    leverages[, 1] = lm.influence(lm.out, do.coef=FALSE)$hat
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
    }
  }
  
  if (length(nonstatVars) > 0 & length(statVars) > 0) { 
    modelType <- ""
    for (var in stationary){
      if( var) {modelType <- paste0(modelType, "G")}
      else {modelType <- paste0(modelType, "L")}
    }
    
    
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

#output = mixedLWR(bandwidth = 80, stationary = c(FALSE, FALSE, TRUE))
#this function seems to be written to require model 1 to always be GGG

megaMaker = function(bandwidths, models, data) {
  megaList = list()
  noResultsGGGInput <- list(ModelType = "GGG", 
                            Coefficients = matrix(NA, nrow = length(data$y), ncol = 3),
                            FittedValues = matrix(NA, nrow = length(data$y), ncol = 3),
                            Leverages = matrix(NA, nrow = length(data$y), ncol = 3))
  megaList[[1]] <- list(noResultsGGGInput, noResultsGGGInput, noResultsGGGInput, 
                  noResultsGGGInput, noResultsGGGInput, noResultsGGGInput, 
                  mixedLWR(bandwidths[length(bandwidths)], models[1,]))

#  megaList[[1]] = list(mixedLWR(bandwidths[length(bandwidths)], models[1,]))
  for (i in 2:dim(models)[1]) {
    megaList[[i]] = lapply(bandwidths, mixedLWR, stationary = models[i, ])
  }
  
  
  ##these name generators can be replaced by just passing them as parameters to the function;
  ##they have to be generated for the results matrix anyway, but calcualting them in the function
  ##does let you run fewer than the max number of models and bandwidths
  modelNames <- c()
  for(modelNum in 1:nrow(models)){
    modelType <- ""
    for (var in models[modelNum,]){
      if( var) {modelType <- paste0(modelType, "G")}
      else {modelType <- paste0(modelType, "L")}
    }
    modelNames <- c(modelNames, modelType)
  }
  
  names(megaList) <- modelNames
  
  bandwidthNames <- c()
  for(bwNum in 1:length(bandwidths)){
    bandwidthNames <- c(bandwidthNames, paste0("Bandwidth ", bwNum," of ", bandwidths[bwNum], " observations"))
  }

  
  for(modelNum in 1:nrow(models)){
    names(megaList[[modelNum]]) <- bandwidthNames
  }
  
  megaList
}

calc.rmse <- function(trueVal, estVal){
  rmse <- (sum((trueVal - estVal)^2)/length(estVal))^.5
  rmse
}

input.rmse <- function(betaToEval, megaList, trueBetaVal, results){
  modelNames <- names(megaList) #get names of models run
  bandwidthNames <- names(megaList[[2]]) #megaList[[1]] will only have 1 bandwidth for the GGG model
  
  for(model in modelNames){
    for(bandwidth in bandwidthNames){
      estCoeff <- megaList[[model]][[bandwidth]][[2]][,(betaToEval + 1)] #requres only the number of the beta
      rmse <- calc.rmse(trueBetaVal, estCoeff)
      results[bandwidth, model, paste0("B", betaToEval, "RMSE")] <- rmse
    }
  }
  results
}


temp = megaMaker(ks, models = models[1:8,], data = mydata)

results <- input.rmse(0, temp, trueB0, results)
results <- input.rmse(1, temp, trueB0, results)
results <- input.rmse(2, temp, trueB0, results)
results
