######
# Set some parameters for the simulation
Total.observations = 2000 # number of observations in the dataset to be analyzed
Error.sd = .3 # standard deviation of error in datra generating process
Bandwidths = c( .1, .4) # proportion of data receiving positives weights in LWR 
Number.of.Bandwidths = length(Bandwidths)

Times = rep(0, Replications)
# Create a loop for each simulation replication
for (replication in 1:Replications) {
  start = Sys.time()
  # We want to implement LWR as described in McMillen and Redfearn, using dataframe DGP1
  DGP1 = DataGenerator(Total.observations, Error.sd)
  
  # Create a loop for each bandwidth used
  for (bandwidth.num in 1:Number.of.Bandwidths) {
    # bandwidth.num = 1
    bandwidth = Bandwidths[bandwidth.num]  
    
    
    LWR.output = lapply(1:Total.observations, LWR, data = DGP1, bandwidth = bandwidth)
#     for (my.observation in 1:Total.observations) {
#       # my.observation = 1
#       LWR.results = LWR(my.observation, bandwidth, DGP1)
#       # Collect the residual and predicted value for my.observation  
# #       residual.results[my.observation] = LWR.results$residuals[my.observation]
# #       predicted.values[my.observation, bandwidth.num] = LWR.results$fitted.values[my.observation]
#     }
    # Now, calculate and collect the Root Mean Square Error
    
    #RMSE.results[RMSE.num, bandwidth.num] = (mean(residual.results^2))^.5   
  } 
  Times[replication] = Sys.time() - start
}
