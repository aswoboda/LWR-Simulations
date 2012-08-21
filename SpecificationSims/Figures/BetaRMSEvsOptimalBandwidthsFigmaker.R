
# The goal of this script is to make a series of graphics comparing the optimal bandwidth and the beta RMSEs
# for the different uberScript parameter combinations

# pick a beta (eventually we want the graphs for all three betas next to each other)...
# horizontal axis is the bandwidth
# vertical axis is the RMSE (make sure to include zero as a reference)
# now plot points for each of four categories
  # GCV, SCV, CV, and bandwidths that minimize RMSE for this beta
  # use color and shape to help differentiate the categories


# load("~/LWR-Simulations/SpecificationSims/uberScriptOutput20120819.RData")
# 
# # playing around with making plots to display optimal bandwidths under GCV vs SCV
# 
# pdf("SpecificationSims/Figures/BetaRMSEvsOptimalBandwidths.pdf")
# par(mfrow = c(3, 3))
# par(oma = c(0, 4, 4.5, 0))
# par(mar = c(3, 3, 1, 1))

for (my.ss in 1:4){
  for (my.error in 1:3) {
    for (my.B2 in 1:3) {
      for (my.B1 in 1:3) {
        my.ss = my.B2 = my.error = my.B1 = 1
        
        # beta 0
        # set plot area - ylim has to include zero and the maximum RMSE score for the three bandwidth selectors
        max.RMSE = max(MetricOutput[my.ss, my.error, my.B1, my.B2, , c("GCV", "SCV", "CV"), "B0.RMSE"])
        plot( 1, 1, type = "n",
              xlim = c(0, as.numeric(dimnames(MetricOutput)$ss[my.ss])), xlab = "",
              ylim = c(0, max.RMSE), ylab = "", 
              main = "", 
              axes = F)
        text(mean(MetricOutput[my.ss, my.error, my.B1, my.B2, , c("GCV"), "bandwidths"]) ,
             mean(MetricOutput[my.ss, my.error, my.B1, my.B2, , c("GCV"), "B0.RMSE"]), "GCV", col = "black" )       
        text(mean(MetricOutput[my.ss, my.error, my.B1, my.B2, , c("SCV"), "bandwidths"]) ,
             mean(MetricOutput[my.ss, my.error, my.B1, my.B2, , c("SCV"), "B0.RMSE"]), "SCV", col = "blue" ) 
        text(mean(MetricOutput[my.ss, my.error, my.B1, my.B2, , c("CV"), "bandwidths"]) ,
             mean(MetricOutput[my.ss, my.error, my.B1, my.B2, , c("CV"), "B0.RMSE"]), "CV", col = "green" ) 
        text(mean(MetricOutput[my.ss, my.error, my.B1, my.B2, , c("RMSE.B0"), "bandwidths"]) ,
             mean(MetricOutput[my.ss, my.error, my.B1, my.B2, , c("RMSE.B0"), "B0.RMSE"]), "B0RMSE", col = "red" )
        axis(1)
        axis(2)
        
        # trying to add the 50 percentile contours insted of all the points to compare the categories...
        
        
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
    mtext(c("GCV", "SCV", "CV"),
          outer = TRUE, line = c(2.5, 1.5, .5), side = 3, cex = .75, at = .8, adj = 0,
          col = c("black", "blue", "green") )

  }
}  
  
dev.off()


