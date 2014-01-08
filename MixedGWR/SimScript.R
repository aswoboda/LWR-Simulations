
source("MixedGWR/helperFunctions.R")

<<<<<<< HEAD
sampleSizes <- c(60, 100) 
B0.SpVar <- c(0) # , 2, 4
B1.SpVar <- c(0) #, 2, 4
B2.SpVar <- c(0) # , 2, 4
errors <- c(0.5) #, 1, 2
numRepeats <- 2
=======
sampleSizes <- c(100, 300, 600, 1000) 
B0.SpVar <- c(0, 1, 3) # 0, 1, 3
B1.SpVar <- c(0, 1, 3) # 0, 1, 3
B2.SpVar <- c(0, 1, 3) # 0, 1, 3
errors <- c(0.5, 1, 2) # 0.5, 1, 2
numRepeats <- 200
>>>>>>> b458d8172901404826b3e26b6c36f3e3688437a8

testParams <- expand.grid(numRepeats = numRepeats, B0SpatialVar = B0.SpVar, B1SpatialVar = B1.SpVar, B2SpatialVar = B2.SpVar, errorSD = errors, sampleSizes = sampleSizes)

x <- mcMultParams(testParams, MC = T)
# saves output as "mcOutputFinal.rds" and ultimately "mcOutputFinal.rds"
