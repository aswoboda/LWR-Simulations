
source("MixedGWR/helperFunctionsNewSCV.R")
set.seed(1234)
sampleSizes <- c(100, 500) 
B0.SpVar <- c(0, 2) # , 2, 4
B1.SpVar <- c(0, 2) #, 2, 4
B2.SpVar <- c(0, 2) # , 2, 4
errors <- c(0.25) #, 1, 2
numRepeats <- 50

testParams <- expand.grid(numRepeats = numRepeats, B0SpatialVar = B0.SpVar, B1SpatialVar = B1.SpVar, B2SpatialVar = B2.SpVar, errorSD = errors, sampleSizes = sampleSizes)

x <- mcMultParams(testParams, MC = F)
# saves output as "mcOutputFinal.rds" and ultimately "mcOutputFinal.rds"

trial2 <- readRDS("mcOutputFinal.rds")
