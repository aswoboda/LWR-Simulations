######

DataGenerator = function(sample.size, error.sd) {
  # Generate a data set like described in McMillen and Redfearn in footnote 7
  # make a data.frame containing "density", "location" and "true.density"
    
  location = runif(sample.size, -20, 20)
  
  density.neg = 11.25 + .5*location
  
  z = 2*pi*location/20
  density.pos = 10 + 1.25*sin(z) + 1.25*cos(z) - .5*location + (.5*location^2)/1000
  
  true.density = density.neg
  true.density[location > 0] = density.pos[location > 0]

  error = rnorm(sample.size, 0, error.sd)
  density = true.density + error
  
  data.frame(location, density, true.density)
}

LWR = function(observation, bandwidth, data) {
  distances = abs(data$location - data$location[observation])
  cut.off.distance = quantile(distances, bandwidth)
  proximity = distances/cut.off.distance
  #Implement our tri-cubic
  weights= ((1 - abs(proximity)^3)^3)
  weights[proximity > 1] = 0
  lm(formula = density ~ location, 
     data = data, 
     weights = weights  )
}


start = Sys.time()

# Set some parameters for the simulation
Total.observations = 2000 # number of observations in the dataset to be analyzed
Error.sd = .3 # standard deviation of error in datra generating process
Bandwidths = c( .1, .4) # proportion of data receiving positives weights in LWR 
Number.of.Bandwidths = length(Bandwidths)
Replications = 1 # number of times we regenerate a dataset for the simulation

# Create a place to store the our results...
# RMSE (Root Mean Square Error) for each replication of the simulation and each bandwidth 
RMSE.results = matrix(-99, Replications, Number.of.Bandwidths)
# residuals (needed to calculate the RMSE)
Residual.results = rep(-999, Total.observations)
# to replcate Figure 1 in McMillen and Redfearn, we'll need the predicted values too
Predicted.values = matrix(-99,Total.observations, Number.of.Bandwidths)

# Create a loop for each simulation replication
for (replication in 1:Replications) {
  # We want to implement LWR as described in McMillen and Redfearn, using dataframe DGP1
  DGP1 = DataGenerator(Total.observations, Error.sd)
  
  # Create a loop for each bandwidth used
  for (bandwidth.num in 1:Number.of.Bandwidths) {
    # bandwidth.num = 1
    bandwidth = Bandwidths[bandwidth.num]  
    
    
    #LWR.output = lapply(1:Total.observations, LWR, data = DGP1, bandwidth = bandwidth)
    for (my.observation in 1:Total.observations) {
      # my.observation = 1
      LWR.results = LWR(my.observation, bandwidth, DGP1)
      # Collect the residual and predicted value for my.observation  
#       residual.results[my.observation] = LWR.results$residuals[my.observation]
#       predicted.values[my.observation, bandwidth.num] = LWR.results$fitted.values[my.observation]
    }
    # Now, calculate and collect the Root Mean Square Error
    
    #RMSE.results[RMSE.num, bandwidth.num] = (mean(residual.results^2))^.5   
  } 
}

end = Sys.time()

Runtime = end - start
print(Runtime)
