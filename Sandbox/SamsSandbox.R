DGPparameters = data.frame(sample.size = 30, 
                            error.sd = .5,
                            B1.spatial.var = 0,
                            B2.spatial.var = 1)


Data = DataGen(DGPparameters$sample.size, 
               DGPparameters$error.sd, 
               DGPparameters$B1.spatial.var, 
               DGPparameters$B2.spatial.var)
output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
LWRinput = Reorganizer(output)


LWRMetrics = function(LWRinput, Data) {
  bandwidths = LWRinput$bandwidths
  if(sd(Data$trueB0) == 0) {
    beta0.cor.results = rep(NA, length(bandwidths))
  } else beta0.cor.results = t(cor(LWRinput$beta0hats, Data$trueB0)) # B0
  
  if(sd(Data$trueB1) == 0) {
    beta1.cor.results = rep(NA, length(bandwidths))
  } else beta1.cor.results = t(cor(LWRinput$beta1hats, Data$trueB1)) #B1
  
  if(sd(Data$trueB2) == 0) {
    beta2.cor.results = rep(NA, length(bandwidths))
  } else beta2.cor.results = t(cor(LWRinput$beta2hats, Data$trueB2))#B2
  
  dep.var.cor.results = t(cor(LWRinput$yhats, Data$dep.var)) #dependent variable
                       
                       
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
                       
                       list(beta1.cor.results = beta1.cor.results, beta2.cor.results = beta2.cor.results, 
                            dep.var.cor.results = dep.var.cor.results, beta0.residuals = beta0.residuals, 
                            beta1.residuals = beta1.residuals, beta2.residuals = beta2.residuals, 
                            beta0.ttest.percent = beta0.ttest.percent, beta1.ttest.percent = beta1.ttest.percent,
                            beta2.ttest.percent = beta2.ttest.percent, gcv.values = gcv.values, 
                            stan.gcv.values = stan.gcv.values, bandwidths = bandwidths)
} 