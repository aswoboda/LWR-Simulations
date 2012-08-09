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

Simulation = function(i, Total.observations, Error.sd, Bandwidths) {
  # this function runs replicates the McMillen and Redfearn simulation
  # the goal is to use this function in "lapply" in order to avoid loops and speed up the code
  # therefore, the first argument is not used, it is just a placeholder for lapply
  
  start = Sys.time()
  # this function assumes the existence of "DataGenerator" and "LWR" functions
  DGP1 = DataGenerator(Total.observations, Error.sd)
  
  Number.of.Bandwidths = length(Bandwidths)
  
  for (bandwidth.num in 1:Number.of.Bandwidths) {
    bandwidth = Bandwidths[bandwidth.num]  
    LWR.output = lapply(1:Total.observations, LWR, data = DGP1, bandwidth = bandwidth)
  }
  end = Sys.time()
  end - start
}
  
# Set some parameters for the simulation
Total.observations = 2000 # number of observations in the dataset to be analyzed
Error.sd = .3 # standard deviation of error in datra generating process
Bandwidths = c( .1, .4) # proportion of data receiving positives weights in LWR 

Replications = 100 # number of times we regenerate a dataset for the simulation

# Create a place to store the our results...
# RMSE (Root Mean Square Error) for each replication of the simulation and each bandwidth 
RMSE.results = matrix(-99, Replications, Number.of.Bandwidths)
# residuals (needed to calculate the RMSE)
Residual.results = rep(-999, Total.observations)
# to replcate Figure 1 in McMillen and Redfearn, we'll need the predicted values too
Predicted.values = matrix(-99, Total.observations, Number.of.Bandwidths)
# the times to get through each iteration of the outer loop
Times = matrix(-99, Replications, 3)


simulation.out = lapply(1:3, Simulation, 
                        Total.observations = Total.observations, 
                        Error.sd = Error.sd, 
                        Bandwidths = Bandwidths   )
