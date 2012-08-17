# empty sandbox


source("SpecificationSims/SimFunctions.R")

# set our simulation parameters
Replications = 10
sample.size = c(50, 100, 200, 500, 1000, 2000)
error.sd = c(3)
B1.spatial.var = 0.25
B2.spatial.var = 0

sim.parameters = expand.grid(sample.size, error.sd, B1.spatial.var, B2.spatial.var)
names(sim.parameters) = c("sample.size", "error.sd", "B1.spatial.var", "B2.spatial.var")

meta.sim.num = dim(sim.parameters)[1]

DGPparameters = sim.parameters[1, ]

simulation(1, DGPparameters)
    

Data = DataGen(DGPparameters$sample.size, 
               DGPparameters$error.sd, 
               DGPparameters$B1.spatial.var, 
               DGPparameters$B2.spatial.var)
output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
new.output = Reorganizer(output)
simMetrics = LWRMetrics(new.output, Data)
optimal.bandwidths = bandwidth.Selector(simMetrics)
Rsquareds = RsquaredComparer(Data)
list(c(optimal.bandwidths, Rsquareds))

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
  
  dep.var.cor.results = cor(LWRinput$yhats, Data$dep.var) #dependent variable
  
  
  # Residuals: For beta0/1/2
  # beta.Residual.calc is a function defined in SimFunctions and can be sourced
  beta0.residuals = beta.Residual.Calc(LWRinput[["beta0hats"]], Data$trueB0)
  beta1.residuals = beta.Residual.Calc(LWRinput[["beta1hats"]], Data$trueB1)
  beta2.residuals = beta.Residual.Calc(LWRinput[["beta2hats"]], Data$trueB2)
  
  # Now calculate for each beta the % of t-tests with values > critical t, for each k.
  beta0.ttest.percent = beta.ttest(LWRinput[["beta0hats"]], LWRinput[["ses0"]], Data$trueB0, LWRinput$bandwidths)
  beta1.ttest.percent = beta.ttest(LWRinput[["beta1hats"]], LWRinput[["ses1"]], Data$trueB1, LWRinput$bandwidths)
  beta2.ttest.percent = beta.ttest(LWRinput[["beta2hats"]], LWRinput[["ses2"]], Data$trueB2, LWRinput$bandwidths)
  
  # Generalized Cross-validation score
  
  gcv.values = GCV(LWRinput[["leverages"]], LWRinput[["yhats"]], Data$dep.var)
  ##Define GCV score
  
  # Standardized CV a la Paez 2007
  stan.gcv.values = standardized.CV(Data$dep.var, LWRinput[["yhats.without"]]) 
  
  data.frame(beta0.cor.results = beta0.cor.results, beta1.cor.results = beta1.cor.results, beta2.cor.results = beta2.cor.results, 
       dep.var.cor.results = dep.var.cor.results, beta0.residuals = beta0.residuals, 
       beta1.residuals = beta1.residuals, beta2.residuals = beta2.residuals, 
       beta0.ttest.percent = beta0.ttest.percent, beta1.ttest.percent = beta1.ttest.percent,
       beta2.ttest.percent = beta2.ttest.percent, gcv.values = gcv.values, 
       stan.gcv.values = stan.gcv.values, bandwidths = bandwidths)
} 



### Problem - right now we are just grabbing the bandwidths that optimize different metrics, 
### we also want to grab the metric values at each of these "optimal" bandwidths to see how they compare

# instead of outputting a vector of bandwidths, will we want a matrix/array?

names(simMetrics)

metric.names = expand.grid(names(optimal.bandwidths), names(optimal.bandwidths))

paste(metric.names[, 1], metric.names[, 2], sep = ".")

metric.output = array(NA, c(length(optimal.bandwidths) + 1, length(optimal.bandwidths)),
                      dimnames = list(value = c("bandwidth", names(optimal.bandwidths)),
                                      optimized = names(optimal.bandwidths)))


temp = data.frame(bandwidth = c(10, 20), gcv = c(200, 190), corB1 = c(.9, .8))

temp2 = data.frame(bandwidth = c(), corB1 = c(), gcv = c())

