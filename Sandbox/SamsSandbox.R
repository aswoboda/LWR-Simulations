source("SpecificationSims/SimFunctions.R")

DGPparameters = data.frame(sample.size = 30, 
                            error.sd = .5,
                            B1.spatial.var = 0.5,
                            B2.spatial.var = 1)


Data = DataGen(DGPparameters$sample.size, 
               DGPparameters$error.sd, 
               DGPparameters$B1.spatial.var, 
               DGPparameters$B2.spatial.var)
output = lapply(1:dim(Data)[1], LWR, Data.Frame = Data)
LWRinput = Reorganizer(output)

LWRMetrics.output = LWRMetrics(LWRinput, Data)

#bandwidth.Selector = function(LWRMetrics.output) {
  bwidth.gcv = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$gcv.values)]
  bwidth.stan.cv = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$stan.gcv.values)]
  
  bwidth.B0.resid = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$beta0.residuals)]
  bwidth.B1.resid = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$beta1.residuals)]
  bwidth.B2.resid = LWRMetrics.output$bandwidths[which.min(LWRMetrics.output$beta2.residuals)]
  
  bwidth.B0.ttest.percent = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$beta0.ttest.percent)]
  bwidth.B1.ttest.percent = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$beta1.ttest.percent)]
  bwidth.B2.ttest.percent = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$beta2.ttest.percent)]
  
  if(is.na(max(LWRMetrics.output$beta1.cor.results)) == T) {
    bwidth.B1.cor.results = NA
  } else bwidth.B1.cor.results = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$beta1.cor.results)]

  if(is.na(max(LWRMetrics.output$beta1.cor.results)) == T) {
   bwidth.B1.cor.results = NA
  } else bwidth.B2.cor.results = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$beta2.cor.results)]
  
if(is.na(max(LWRMetrics.output$beta1.cor.results)) == T) {
   bwidth.B1.cor.results = NA
  } else bwidth.dep.var.cor.results = LWRMetrics.output$bandwidths[which.max(LWRMetrics.output$dep.var.cor.results)]
  
  c(GCV = bwidth.gcv, SCV = bwidth.stan.cv,
    SSRB0 = bwidth.B0.resid, SSRB1 = bwidth.B1.resid, SSRB2 = bwidth.B2.resid, 
    "ttest%B0" = bwidth.B0.ttest.percent, 
    "ttest%B1" = bwidth.B1.ttest.percent, 
    "ttest%B2" = bwidth.B2.ttest.percent,
    corB1 = bwidth.B1.cor.results, corB2 = bwidth.B2.cor.results,
    corDepVar = bwidth.dep.var.cor.results)
#}