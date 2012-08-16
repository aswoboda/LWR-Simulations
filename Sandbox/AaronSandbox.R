


begin = Sys.time()

end = Sys.time()

times = end - begin

class(times)

paste("time dif", print(times))

Replications = 500
sample.size = c(50, 100, 200, 500)
error.sd = c(.3, 1)
B1.spatial.var = c(.5, 1)
B2.spatial.var = 1
 
# expand the parameter vectors and create a container for our simulation output
sim.parameters = expand.grid(sample.size, error.sd, B1.spatial.var, B2.spatial.var)
names(sim.parameters) = c("sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var")
sim.parameters
