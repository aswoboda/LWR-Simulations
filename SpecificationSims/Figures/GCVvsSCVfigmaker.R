

load("~/LWR-Simulations/SpecificationSims/uberScriptOutput.RData")

# playing around with making plots to display optimal bandwidths under GCV vs SCV

pdf("SpecificationSims/Figures/GCVvsSCVscatterplots.pdf")
par(mfrow = c(3, 3))
par(oma = c(0, 4, 4.5, 0))
par(mar = c(3, 3, 1, 1))

for (my.ss in 1:4){
  for (my.B2 in 1:3) {
    for (my.error in 1:3) {
      for (my.B1 in 1:3) {
        #my.ss = my.B2 = my.error = my.B1 = 1
        plot(MetricOutput[my.ss, my.error, my.B1, my.B2, , "GCV", "bandwidths"],
             MetricOutput[my.ss, my.error, my.B1, my.B2, , "SCV", "bandwidths"],
           xlim = c(0, 500), xlab = "",
           ylim = c(0, 500), ylab = "", 
           main = "", #paste("mSCV = ", round(mean(simOutput[k, i, j, 1, , "SCV"]), 0),
           #  "mGCV = ", round(mean(simOutput[k, i, j, 1, , "GCV"]), 0)),
           pch = 16, cex = .65,
           col = rgb(red = 0, green = 0, blue = 1, alpha = .05),
           axes = F
      )
      ifelse(my.error == 1 & my.B1 == 1, axis(1), axis(1, labels = F))
      ifelse(my.error == 1 & my.B1 == 1, axis(2), axis(2, labels = F))
      if(my.error == 1 & my.B1 == 1) mtext("GCV", side = 1, line = 2, cex = .8)
      if(my.error == 1 & my.B1 == 1) mtext("SCV", side = 2, line = 2, cex = .8)
      }
    }
  mtext(paste("GCV vs. SCV Optimal Bandwidths (ss =", dimnames(MetricOutput)$ss[my.ss], ")"), 
        outer = TRUE, line = 3, side = 3, font = 2, cex = 1.5)
  mtext(paste("Beta 2 = ", dimnames(MetricOutput)$B2sv[my.B2], ", Beta 1 Spatial Variation Parameter = ..."), 
        outer = TRUE, line = 1.5, at = .5, col = "red", font = 2)
  mtext(dimnames(MetricOutput)$B1sv, 
        outer = TRUE, line = 0, at = c(.2, .55, .85), col = "red", font = 2)
  mtext("Standard Deviation of DGP Error Term", 
        outer = TRUE, line = 2, at = .5, side = 2, col = "red", font = 2)
  mtext(dimnames(MetricOutput)$error.sd, 
        outer = TRUE, line = 0.5, at = c(.8, .5, .3), side = 2, col = "red", font = 2)
  }
}  
  
dev.off()
