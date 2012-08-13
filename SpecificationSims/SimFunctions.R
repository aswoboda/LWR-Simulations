

DataGen = function(sample.size, error.sd, B1.spatial.var, B2.spatial.var) {
  # Set sample size
  n = sample.size
  
  # Give observations coordinates 
  east = runif(sample.size)*10 
  north = runif(sample.size)*10
  indep.var1 = runif(sample.size)*10 # random indep.var1's
  indep.var2 = runif(sample.size)*10 # random indep.var2's
  
  trueB0 = 0
  trueB1 = B1.spatial.var*north/4 + 1 - .5*B1.spatial.var
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
  kvector <- seq(minimumk, sample.size - 1, up.by)
  if (sample.size %% up.by != 1)  kvector = c(kvector, sample.size - 1)  #!= does not equal
  numK <- length(kvector)
  numBetas = 3
  
  
  # Creates containers for our parameters/metrics. 
  temp.est.betas = matrix(-99, numBetas, numK) # Need a matrix, row for each B, columns for each k
  temp.st.errors = matrix(-99, numBetas, numK) # Same as above
  temp.est.dep.var = matrix(-99, 1, numK ) # Matrix to be consistent with above, but only 1 value per k
  temp.leverage = matrix(-99, 1, numK) # Same as temp.est.dep.var
  temp.est.dep.var.without = matrix(-99, 1, numK)
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
    
    #Now we are going to exclude the observation itself.
    Data.Frame$Weights[my.observation] = 0
    lmreg = lm(my.model, data = Data.Frame, weights = Weights)
    
    temp.est.dep.var.without[j] = lmreg$fitted.values[my.observation] 
  }
  list(betas = temp.est.betas, st.errors = temp.st.errors, dep.vars = temp.est.dep.var, leverages = temp.leverage,
       dep.vars.without = temp.est.dep.var.without, bandwidths = kvector)
}

Reorganizer = function(lapplyoutput) {
  ### Write a function that takes as input the output from lapply(1:n, LWR, ...) 
  ### and reorganizes from a list of items for each observations
  ### into a list with an item for each type of thing we want to compare ...
  ### for instance, betahats for each variable, se's for each beta, dependent var est, leverage

  n = length(lapplyoutput) # essentially the number of observations from our dataset we ran LWR on
  ks = dim(lapplyoutput[[1]][[1]])[2] # should give us the number of ks we used running LWR
  # betas
  temp = sapply(lapplyoutput, "[", 1) # grabbing the estimated beta values
  temp1 = unlist(temp)
  beta0hats = matrix(temp1[seq(1, length(temp1), 3)], n, ks, byrow = T)
  beta1hats = matrix(temp1[seq(2, length(temp1), 3)], n, ks, byrow = T)
  beta2hats = matrix(temp1[seq(3, length(temp1), 3)], n, ks, byrow = T)
  
  # standard errors
  temp = sapply(lapplyoutput, "[", 2) # grabbing the estimated beta values
  temp1 = unlist(temp)
  ses0 = matrix(temp1[seq(1, length(temp1), 3)], n, ks, byrow = T)
  ses1 = matrix(temp1[seq(2, length(temp1), 3)], n, ks, byrow = T)
  ses2 = matrix(temp1[seq(3, length(temp1), 3)], n, ks, byrow = T)
  
  # dependent variable estimates with observation
  temp = sapply(lapplyoutput, "[", 3) # grabbing the estimated dependent variable values
  temp1 = unlist(temp)
  yhats = matrix(temp1, length(temp1)/ks, ks, byrow = T)
  
  # dependent variable estimates without observation
  temp = sapply(lapplyoutput, "[", 5) # grabbing the estimated dependent variable values
  temp1 = unlist(temp)
  yhats.without = matrix(temp1, length(temp1)/ks, ks, byrow = T)
  
  # leverage values
  temp = sapply(lapplyoutput, "[", 4) # grabbing the estimated dependent variable values
  temp1 = unlist(temp)
  leverages = matrix(temp1, length(temp1)/ks, ks, byrow = T)
  
  # put everything together as output for the function
  list(beta0hats = beta0hats, beta1hats = beta1hats, beta2hats = beta2hats,
       ses0 = ses0, ses1 = ses1, ses2 = ses2,
       yhats = yhats, leverages = leverages, yhats.without = yhats.without, bandwidths = output[[1]]$bandwidths)
}

beta.Residual.Calc = function(betahats, truebetas) {
  
  colSums((betahats - truebetas)^2)
}

# We need to get a better name for this function. It is doing more than just performing a t-test..
beta.ttest = function(betahats, ses, truebetas) {
  # Want to test for significance the difference between each betahat and its corresponding truebeta.
  kvector <- seq(minimumk, sample.size - 1, up.by)
  if (sample.size %% up.by != 1)  kvector = c(kvector, sample.size - 1)
  
  t.percent = c() # Flexible length vector
  
  t.obs = ((betahats - truebetas)/ses)
  critical.ts = qt(.975, kvector-2) # .975 = our significance level, kvector-2 tells us our DoF, K+observation-3 betas
  for (j in 1: length(critical.ts)) {
    t.percent[j] = sum(t.obs[ , j] > critical.ts[j])/dim(t.obs)[1]
  } 
  t.percent
}

# Generalized Cross Validation for each k
GCV = function(leverages, yhats, dep.var) {
  sample.size = dim(yhats)[1]
  v1 <- colSums(leverages)
  SE <- colSums((dep.var-yhats)^2)
  gcv <- sample.size*SE/(sample.size-v1)^2   
  gcv
}

# Standardized CV from Paez et all
# Need to make sure that we are supposed to square denominator
standardized.CV = function(dep.var, yhats.without) {
  numer = ((dep.var - yhats.without)^2)
  denom = rowSums(numer) 
  stan.CV.values = colSums(numer/denom)
}

LWRMetrics = function(LWRinput, Data) {
  bandwidths = LWRinput$bandwidths
  beta1.cor.results = t(cor(LWRinput$beta1hats, Data$trueB1))#B1
  beta2.cor.results = t(cor(LWRinput$beta2hats, Data$trueB2)) #B2
  dep.var.cor.results = t(cor(LWRinput$yhats, Data$dep.var)) #dependent variable
  
  
  # Residuals: For beta0/1/2
  # beta.Residual.calc is a function defined in SimFunctions and can be sourced
  beta0.residuals = beta.Residual.Calc(LWRinput[["beta0hats"]], Data$trueB0)
  beta1.residuals = beta.Residual.Calc(LWRinput[["beta1hats"]], Data$trueB1)
  beta2.residuals = beta.Residual.Calc(LWRinput[["beta2hats"]], Data$trueB2)
  
  # Now calculate for each beta the % of t-tests with values > critical t, for each k.
  beta0.ttest.percent = beta.ttest(LWRinput[["beta0hats"]], LWRinput[["ses0"]], Data$trueB0)
  beta1.ttest.percent = beta.ttest(LWRinput[["beta1hats"]], LWRinput[["ses1"]], Data$trueB1)
  beta2.ttest.percent = beta.ttest(LWRinput[["beta2hats"]], LWRinput[["ses2"]], Data$trueB2)
  
  # Generalized Cross-validation score
  
  gcv.values = GCV(LWRinput[["leverages"]], LWRinput[["yhats"]], Data$dep.var)
  ##Define GCV score
  
  # Standardized CV a la Paez 2007
  stan.gcv.values = standardized.CV(Data$dep.var, LWRinput[["yhats.without"]]) 
  
  list(beta1.cor.results = beta1.cor.results, beta2.cor.results = beta2.cor.results, 
       dep.var.cor.results = dep.var.cor.results, beta0.residuals = beta0.residuals, 
       beta1.residuals = beta1.residuals, beta2.residuals = beta2.residuals, 
       beta0.ttest.percent = beta0.ttest.percent, beta1.ttest.percent = beta1.ttest.percent,
       beta2.ttest.percent = beta2.ttest.percent, gcv.values = gcv.values, 
       stan.gcv.values = stan.gcv.values, bandwidths = bandwidths)
} 

min.Generator = function(LWRMetrics.output) {
  min.bandwidth.gcv = LWRMetrics.output$bandwidths[which.min(LWRMetrics$gcv.values)]
  min.bandwidth.stan.gcv = LWRMetrics.output$bandwidths[which.min(LWRMetrics$stan.values)]
  
}