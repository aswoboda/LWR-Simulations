

DataGen = function(sample.size, error.sd, B1.spatial.var, B2.spatial.var) {
  # Set sample size
  n = sample.size
  
  # Give observations coordinates 
  east = runif(sample.size)*10 
  north = runif(sample.size)*10
  indep.var1 = runif(sample.size)*10 # random indep.var1's
  indep.var2 = runif(sample.size)*10 # random indep.var2's
  
  trueB0 = 0
  trueB1 = B1.spatial.var*north/4 + 1 - 5*B1.spatial.var
  trueB2 = B2.spatial.var*5 + 1 - B2.spatial.var*east
  
  
  error=rnorm(sample.size, 0, error.sd)
  
  dep.var = trueB0 + indep.var1*trueB1+indep.var2*trueB2 + error
  
 output = data.frame(dep.var, north, east, indep.var1, indep.var2, trueB0, trueB1, trueB2)
 output
 
}




# LWR for all Ks - specify a model as a parameter
# Generate weights, proximities

# Model in quotes is default model, don't need anything entered for it to run
LWR = function( my.observation, Data.Frame, my.model = "dep.var ~ indep.var1 + indep.var2") {
  #my.observation = 2
  #Data.Frame = test.data
  #my.model = "dep.var ~ indep.var1 + indep.var2"
  sample.size = dim(Data.Frame)[1]
  
  # Creates a vector of ks for each observation.
  up.by = 5
  minimumk = 5
  kvector <- c(seq(minimumk, sample.size - 1, up.by), sample.size-1) 
  numK <- length(kvector)
  numBetas = 3
  
  
  # Creates containers for our parameters/metrics. 
  temp.est.betas = matrix(-99, numBetas, numK) # Need a matrix, row for each B, columns for each k
  temp.st.errors = matrix(-99, numBetas, numK) # Same as above
  temp.est.dep.var = matrix(-99, 1, numK ) # Matrix to be consistent with above, but only 1 value per k
  temp.leverage = matrix(-99, 1, numK) # Same as temp.est.dep.var
  
  # Calculate distance
  
    Di<-((Data.Frame$north-Data.Frame$north[my.observation])^2+(Data.Frame$east-Data.Frame$east[my.observation])^2)^.5
  # This loop may be eventually turned into an lapply function. 
  for (j in 1:numK) { # j is the position of our k in the kvector
   
  k <- kvector[j]
    
    # Calculate the appropriate weights for the observations for each k, using
    # previously calculated distance
    threshold = sort(Di)[k+1] # b is the threshold distance (distance to the k+1 th nearest observation to obs i)
    Weights = (1-(Di/threshold)^2)^2
    Weights[Di>threshold] = 0 
    Data.Frame$Weights = Weights
    
    lmreg = lm(my.model, data = Data.Frame, weights = Weights)
    
    
    temp.est.betas[,j] <- lmreg$coefficients # keep track of the coefficient estimate
    temp.st.errors[,j] <- summary(lmreg)$coefficients[,2]	 # keep track the coefficient st error
    temp.est.dep.var[j] <- lmreg$fitted.values[my.observation] # keep track of the predicted value of y
    temp.leverage[j] <- lm.influence(lmreg)$hat[as.character(my.observation)] # the leverage value
    
  }
  list(betas = temp.est.betas, st.errors = temp.st.errors, dep.vars = temp.est.dep.var, leverages = temp.leverage)
}

