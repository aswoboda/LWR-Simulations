

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
  trueB2 = B2.spatial.var*east/4 + 1 - .5*B2.spatial.var
  
  
  error=rnorm(sample.size, 0, error.sd)
  
  dep.var = trueB0 + indep.var1*trueB1+indep.var2*trueB2 + error
  
 output = data.frame(dep.var, north, east, indep.var1, indep.var2, trueB0, trueB1, trueB2, error)
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
       yhats = yhats, leverages = leverages, yhats.without = yhats.without, bandwidths = lapplyoutput[[1]]$bandwidths)
}

RMSE.beta.Calc = function(betahats, truebetas) {
  sample.size = length(truebetas)[1]
  colSums((betahats - truebetas)^2)/(sample.size - 1)
}

# We need to get a better name for this function. It is doing more than just performing a t-test..
beta.ttest = function(betahats, ses, truebetas, bandwidths) {
  # Want to test for significance the difference between each betahat and its corresponding truebeta.
  
  t.percent = c() # Flexible length vector
  
  t.obs = ((betahats - truebetas)/ses)
  critical.ts = qt(.975, bandwidths-2) # .975 = our significance level, kvector-2 tells us our DoF, K+observation-3 betas
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

# Row standardized CV from Paez et all
# Need to make sure that we are supposed to square denominator
row.standardized.CV = function(dep.var, yhats.without) {
  numer = ((dep.var - yhats.without)^2)
  denom = rowSums(numer) 
  stan.CV.values = colSums(numer/denom)
}
# Regular CV
regular.CV = function(dep.var, yhats.without) {
  reg.CV.values = colSums((dep.var - yhats.without)^2)
}

# AICc, as suggested by Paez and Fotheringham as an alternative to CVs
AICc.calc = function(Reorg.output, Data) {
  sample.size = dim(Reorg.output$leverages)[1]
  v1 = colSums(Reorg.output$leverages)
  est.error.sd = apply(Data$dep.var - Reorg.output$yhats, 2, sd)
  
  AICc.values = 2*sample.size*log(est.error.sd) + sample.size*log(2*pi) + 
    sample.size*((sample.size + v1)/(sample.size - 2 - v1))
}

# R2 for optimal model 
R2.calc = function(LWRoutput, Data) {
  
  R2 = 1 - (var(Data$dep.var - LWRoutput$yhats)/var(Data$dep.var))
  R2.output = diag(R2)
}

LWRMetrics = function(LWRinput, Data) {
  bandwidths = LWRinput$bandwidths
  if(sd(Data$trueB0) == 0) {
    beta0.cor.results = rep(NA, length(bandwidths))
  } else beta0.cor.results = cor(LWRinput$beta0hats, Data$trueB0) # B0
  
  if(sd(Data$trueB1) == 0) {
    beta1.cor.results = rep(NA, length(bandwidths))
  } else beta1.cor.results = cor(LWRinput$beta1hats, Data$trueB1) #B1
  
  if(sd(Data$trueB2) == 0) {
    beta2.cor.results = rep(NA, length(bandwidths))
  } else beta2.cor.results = cor(LWRinput$beta2hats, Data$trueB2)#B2
  
  # Residuals: For betas
  # beta.Residual.calc is a function defined in SimFunctions and can be sourced
  beta0.RMSE = RMSE.beta.Calc(LWRinput[["beta0hats"]], Data$trueB0)
  beta1.RMSE = RMSE.beta.Calc(LWRinput[["beta1hats"]], Data$trueB1)
  beta2.RMSE = RMSE.beta.Calc(LWRinput[["beta2hats"]], Data$trueB2)
  
  # Now calculate for each beta the % of t-tests with values > critical t, for each k.
  beta0.ttest.percent = beta.ttest(LWRinput[["beta0hats"]], LWRinput[["ses0"]], Data$trueB0, LWRinput$bandwidths)
  beta1.ttest.percent = beta.ttest(LWRinput[["beta1hats"]], LWRinput[["ses1"]], Data$trueB1, LWRinput$bandwidths)
  beta2.ttest.percent = beta.ttest(LWRinput[["beta2hats"]], LWRinput[["ses2"]], Data$trueB2, LWRinput$bandwidths)
  
  # Generalized Cross-validation score
  gcv.values = GCV(LWRinput[["leverages"]], LWRinput[["yhats"]], Data$dep.var)
  
  # Regular CV score
  reg.cv.values = regular.CV(Data$dep.var, LWRinput[["yhats.without"]]) 
  
  # Standardized CV a la Paez 2007
  row.stan.gcv.values = row.standardized.CV(Data$dep.var, LWRinput[["yhats.without"]]) 
  
  # AICc
  AICc.values = AICc.calc(LWRinput, Data)
  
  # R^2 
  R2.values = R2.calc(LWRinput, Data)
  
  data.frame(B0.cor = beta0.cor.results, 
             B1.cor = beta1.cor.results, 
             B2.cor = beta2.cor.results, 
             B0.RMSE = beta0.RMSE, 
             B1.RMSE = beta1.RMSE, 
             B2.RMSE = beta2.RMSE, 
             B0.t.perc = beta0.ttest.percent, 
             B1.t.perc = beta1.ttest.percent, 
             B2.t.perc = beta2.ttest.percent, 
             GCV = gcv.values, 
             SCV = row.stan.gcv.values, 
             CV = reg.cv.values, 
             AICc = AICc.values, 
             R2 = R2.values, 
             bandwidths = bandwidths)
} 

bandwidth.Selector = function(LWRMetrics.output) {
  bwidth.gcv = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$GCV)]
  bwidth.row.stan.cv = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$SCV)]
  bwidth.reg.cv = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$CV)]
  bwidth.AICc = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$AICc)]
  # We think that if it ends up that we do not optimize over R2 and perhaps other metrics, 
  # the numbers will be passed along. 
  bwidth.R2 = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$R2)]
  
  bwidth.B0.RMSE = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$B0.RMSE)]
  bwidth.B1.RMSE = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$B1.RMSE)]
  bwidth.B2.RMSE = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$B2.RMSE)]
  
  bwidth.B0.ttest.percent = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$B0.t.perc)]
  bwidth.B1.ttest.percent = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$B1.t.perc)]
  bwidth.B2.ttest.percent = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$B2.t.perc)]
  
  if(is.na(max(LWRMetrics.output$B0.cor)) == T) {
    bwidth.B0.cor = NA
   } else bwidth.B0.cor = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$B0.cor)]
 
  if(is.na(max(LWRMetrics.output$B1.cor)) == T) {
    bwidth.B1.cor = NA
  } else bwidth.B1.cor = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$B1.cor)]
  
  if(is.na(max(LWRMetrics.output$B2.cor)) == T) {
    bwidth.B2.cor = NA
  } else bwidth.B2.cor = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$B2.cor)]
  
  
  c(GCV = bwidth.gcv, SCV = bwidth.row.stan.cv, CV = bwidth.reg.cv, AICc = bwidth.AICc,
    R2 = bwidth.R2, RMSE.B0 = bwidth.B0.RMSE, RMSE.B1 = bwidth.B1.RMSE, RMSE.B2 = bwidth.B2.RMSE, 
    "ttest%B0" = bwidth.B0.ttest.percent, 
    "ttest%B1" = bwidth.B1.ttest.percent, 
    "ttest%B2" = bwidth.B2.ttest.percent,
    corB0 = bwidth.B0.cor, corB1 = bwidth.B1.cor, corB2 = bwidth.B2.cor
    )
}

RsquaredComparer = function(Data) {
  var.y = var(Data$dep.var)
  var.error = var(Data$error)
  
  ols = lm(dep.var ~ indep.var1 + indep.var2, data = Data)
  ols.sum = summary(ols)
  c(R2OLS = ols.sum$r.squared, R2LWR = (1- var.error/var.y))
}

simulation = function(iteration, DGPparameters) {
  
  Data = DataGen(DGPparameters$sample.size, 
                 DGPparameters$error.sd, 
                 DGPparameters$B1.spatial.var, 
                 DGPparameters$B2.spatial.var)
  output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
  new.output = Reorganizer(output)
  simMetrics = LWRMetrics(new.output, Data)
  optimal.bandwidths = bandwidth.Selector(simMetrics)
  temp = data.frame(bandwidths = optimal.bandwidths, optimizing = names(optimal.bandwidths))
  temp2 = merge(temp, simMetrics, all.x = T)
  
  metrics = temp2[order(temp2$optimizing), ]
  Rsquareds = RsquaredComparer(Data)
  list(Rsquareds, metrics)
}

simulationReplicator = function(N = 2, DGPparameters, MC = FALSE){
  require(multicore, quietly = TRUE)
  if(MC == TRUE) {
    temp = mclapply(1:N, simulation, DGPparameters = DGPparameters)
  }
  else temp = lapply(1:N, simulation, DGPparameters = DGPparameters)
  temp
}

simRepReorganizer = function(simRepOut){
  reps = length(simRepOut)
  # grab all the rsquared values and put them into a matrix
  test = sapply(simRepOut, "[", 1)
  rsquared.matrix = matrix(unlist(test), reps, 2, byrow = T)
  
  # grab all the metric matrices and put them into an array
  test2 = sapply(simRepOut, "[", 2)
  metric.array = array(NA, c(reps, dim(test2[[1]])[1], dim(test2[[1]])[2]-1),
                       dimnames = list(replication = 1:reps,
                                       optimized = test2[[1]]$optimizing,
                                       metric.values = names(test2[[1]][-2])))
  
  for (i in 1:reps) {
    metric.array[i, , ] = as.matrix(test2[[i]][, -2])
  }
  
  # put the two together into a list as the final output
  list(rsquared.matrix, metric.array)
}

