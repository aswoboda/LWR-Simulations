
source("SpecificationSims/SimFunctions.R")

# write a function that takes as given some simulation parameters, and runs the these commands a number of times
# so that we can start to see if there are patterns (which metrics suggest similar bandwidths, etc.)
#

# Need print, paste
# Paste progress in a .Rout file
meta.sim.num = 12

for(i in  1:dim(sim.parameters)[1]) { 
  start = Sys.time()
  # simulationReplicator(sim.parameters, MC = FALSE)
 # num = 2^2^2^2 + 3^3^3
  end = Sys.time()
  time = end - start
  print(paste("For loop", i,"of", meta.sim.num, "it took", time))
  save()
}