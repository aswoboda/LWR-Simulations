# Thinking about avoiding the massive nested loops in the simulation 
# and instead trying to expand a grid of parameter values to use in the mclapply function

parameters1 = c(1:4)
parameters2 = c(10:12)
parameters3 = letters[1:3]

parameters = expand.grid(parameters1, parameters2, parameters3)

testfun = function(i, data) {
  output = paste(data[i, ])
  output
}

lapply(1:36, testfun, data = parameters)

# to use the expand.grid and lapply for the simulations... we'll want a sim function 
# that expects a data.frame of parameters and is told which row of the parameters to use
