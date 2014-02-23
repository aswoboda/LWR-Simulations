
source("MixedGWR/helperFunctionsNewSCV.R")
set.seed(123452)
sampleSizes <- c(50, 100, 200, 400, 800) #
B0.SpVar <- c(0, 2, 4) # 
B1.SpVar <- c(0, 2, 4) #
B2.SpVar <- c(0, 2, 4) # 
errors <- c(0.25, .5, 1, 2, 3) #
numRepeats <- 100

testParams <- expand.grid(numRepeats = numRepeats, B0SpatialVar = B0.SpVar, B1SpatialVar = B1.SpVar, B2SpatialVar = B2.SpVar, errorSD = errors, sampleSizes = sampleSizes)
# testParams = testParams[which(testParams$B0SpatialVar == testParams$B1SpatialVar), ]
# testParams = testParams[which(testParams$B0SpatialVar == testParams$B2SpatialVar), ]


x <- mcMultParams(testParams, MC = T)
# saves output as "mcOutputFinal.rds" and ultimately "mcOutputFinal.rds"

#table(x["SCV", 1, ])
