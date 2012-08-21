

load("~/LWR-Simulations/SpecificationSims/uberScriptOutput.RData")

# playing around with making plots to display optimal bandwidths under GCV vs SCV

pdf("SpecificationSims/Figures/OptimalBandwidths.pdf")
par(mfrow = c(3, 3))
par(oma = c(0, 4, 4.5, 0))
par(mar = c(3, 3, 1, 1))

for (my.ss in 1:4){
  for (my.error in 1:3) {
    for (my.B2 in 1:3) {
      for (my.B1 in 1:3) {
        #my.ss = my.B2 = my.error = my.B1 = 1
        GCVdens = density(MetricOutput[my.ss, my.error, my.B1, my.B2, , "GCV", "bandwidths"], bw = as.numeric(dimnames(MetricOutput)$ss[my.ss])/13)
        SCVdens = density(MetricOutput[my.ss, my.error, my.B1, my.B2, , "SCV", "bandwidths"], bw = as.numeric(dimnames(MetricOutput)$ss[my.ss])/13)
        CVdens  = density(MetricOutput[my.ss, my.error, my.B1, my.B2, , "CV", "bandwidths"] , bw = as.numeric(dimnames(MetricOutput)$ss[my.ss])/13)
        plot( 1, 1, type = "n",
           xlim = c(0, as.numeric(dimnames(MetricOutput)$ss[my.ss])), xlab = "",
           ylim = c(0, max(c(GCVdens$y, SCVdens$y, CVdens$y))), ylab = "", 
           main = "", 
           axes = F
      )
        lines(GCVdens, col = "black")
        lines(SCVdens, col = "blue")
        lines(CVdens, col = "green")
        if(my.B2 == 1) mtext(paste("B1sv=", dimnames(MetricOutput)$B1sv[my.B1]), side = 3, cex = .8, col = "red")
        if(my.B1 == 1) mtext(paste("B2sv=", dimnames(MetricOutput)$B2sv[my.B2]), side = 2, line = 2, , cex = .8, col = "red")
      #ifelse(my.error == 1 & my.B1 == 1, axis(2), axis(2, labels = F))
        axis(1)
        if(my.B2 == 1 & my.B1 == 1) {
          mtext("# of obs. in bandwidth", side = 1, line = 2, cex = .8)
          mtext("Relative Frequency", side = 2, line = .5, cex = .8)
          mtext("Global Model", side = 3, line = -2, cex = .5)
        }
#         mtext(paste("B1sv=", dimnames(MetricOutput)$B1sv[my.B1],
#                     "B2sv=", dimnames(MetricOutput)$B2sv[my.B2]), 
#               side = 3, line = 0, cex = .5)
        
        # this code was written to compare the R^2 values across the models, but the R2 values cannot be trusted right now
#         aveR2ols = mean(R2Output[my.ss, my.error, my.B1, my.B2, , "OLS"])
#         aveR2lwr = mean(R2Output[my.ss, my.error, my.B1, my.B2, , "LWR"])
#         mtext(paste("True =", round(aveR2lwr, 2),
#                     "OLS =", round(aveR2ols, 2)),
#               side = 3, line = -1, cex = .5)
      }
    }
    mtext(paste("Optimal Bandwidths \n (ss =", dimnames(MetricOutput)$ss[my.ss], 
                "error sd =", dimnames(MetricOutput)$error.sd[my.error], ")"), 
          outer = TRUE, line = 1, side = 3, font = 2, cex = 1)

  }
}  
  
dev.off()


