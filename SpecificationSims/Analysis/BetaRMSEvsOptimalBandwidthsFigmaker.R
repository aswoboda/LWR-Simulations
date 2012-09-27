
# The goal of this script is to make a series of graphics comparing the optimal bandwidth and the beta RMSEs
# for the different uberScript parameter combinations

# pick a beta (eventually we want the graphs for all three betas next to each other)...
# horizontal axis is the bandwidth
# vertical axis is the RMSE (make sure to include zero as a reference)
# now plot points for each of four categories
  # GCV, SCV, CV, and bandwidths that minimize RMSE for this beta
  # use color and shape to help differentiate the categories

source("SpecificationSims/Figures/FigFunctions.R")
# load("~/LWR-Simulations/SpecificationSims/uberScriptOutput20120819.RData")
# 
# # playing around with making plots to display optimal bandwidths under GCV vs SCV
# 
pdf("SpecificationSims/Figures/BetaRMSEvsOptimalBandwidths.pdf")
par(mfrow = c(3, 3))
par(oma = c(0, 2, 4.5, 0.5))
par(mar = c(3, 3, 1, 1))

for (my.ss in 1:4){
  for (my.error in 1:3) {
    for (my.B2 in 1:3) {
      for (my.B1 in 1:3) {
        #my.ss = my.B1 = my.error = my.B2 = 1
        
        # beta 
        # set plot area - ylim has to include zero and the maximum RMSE score for the three bandwidth selectors
        max.RMSE = quantile(MetricOutput[my.ss, , my.B1, my.B2, , c("GCV", "SCV", "CV"), "B2.RMSE"], .98)

        prob = .8 # the proportion of data I want included in the contour plot below
        # GCV
        GCVbw = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("GCV"), "bandwidths"]
        GCVrmse = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("GCV"), "B2.RMSE"]
        GCV.contour = peel(GCVbw, GCVrmse, probs = prob)[[1]]
        
        
        # SCV
        SCVbw = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("SCV"), "bandwidths"]
        SCVrmse = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("SCV"), "B2.RMSE"]
        SCV.contour = peel(SCVbw, SCVrmse, probs = prob)[[1]]
        
        
        # CV
        CVbw = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("CV"), "bandwidths"]
        CVrmse = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("CV"), "B2.RMSE"]
        CV.contour = peel(CVbw, CVrmse, probs = prob)[[1]]
        
        
        # RMSE
        RMSEbw = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("RMSE.B2"), "bandwidths"]
        RMSErmse = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("RMSE.B2"), "B2.RMSE"]
        RMSE.contour = peel(RMSEbw, RMSErmse, probs = prob)[[1]]
        
        
        plot( 1, 1, type = "n",
              xlim = c(0, as.numeric(dimnames(MetricOutput)$ss[my.ss])), xlab = "",
              ylim = c(0, max.RMSE), ylab = "", 
              main = "", 
              axes = F)
        polygon(GCVbw[GCV.contour], GCVrmse[GCV.contour],  col = rgb(.5, .5, .5, alpha= .25), border = "black")
        polygon(SCVbw[SCV.contour], SCVrmse[SCV.contour],  col = rgb(0, 0, 1, alpha= .25), border = "blue")
        polygon(CVbw[CV.contour], CVrmse[CV.contour],  col = rgb(0, 1, 0, alpha= .25), border = "green")
        polygon(RMSEbw[RMSE.contour], RMSErmse[RMSE.contour],  col = rgb(1, 0, 0, alpha= .25), border = "red")
        
        points(mean(GCVbw) ,
               mean(GCVrmse), pch = 16, col = "black" )       
        points(mean(SCVbw) ,
               mean(SCVrmse), pch = 16, col = "blue" ) 
        points(mean(CVbw) ,
               mean(CVrmse), pch = 16, col = "green" ) 
        points(mean(RMSEbw) ,
               mean(RMSErmse), pch = 16, col = "red" )
        axis(1)
        axis(2)
           
        if(my.B2 == 1) mtext(paste("B1sv=", dimnames(MetricOutput)$B1sv[my.B1]), side = 3, cex = .8, col = "red")
        if(my.B1 == 1) mtext(paste("B2sv=", dimnames(MetricOutput)$B2sv[my.B2]), side = 2, line = 2, , cex = .8, col = "red")
      #ifelse(my.error == 1 & my.B2 == 1, axis(2), axis(2, labels = F))
        if(my.B1 == 1 & my.B2 == 1) {
          mtext("# of obs. in bandwidth", side = 1, line = 2, cex = .8)
         # mtext("Relative Frequency", side = 2, line = .5, cex = .8)
          mtext("Global Model", side = 3, line = -2, cex = .5)
        }
      }
    }
    mtext(paste("B2 RMSE vs. Bandwidths \n (ss =", dimnames(MetricOutput)$ss[my.ss], 
                "error sd =", dimnames(MetricOutput)$error.sd[my.error], ")"), 
          outer = TRUE, line = 1, side = 3, font = 2, cex = 1)
    mtext(c("GCV", "SCV", "CV"),
          outer = TRUE, line = c(2.5, 1.5, .5), side = 3, cex = .75, at = .8, adj = 0,
          col = c("black", "blue", "green") )

  }
}  
  
dev.off()


