
# The goal of this script is to make a series of graphics comparing the optimal bandwidth and the beta RMSEs
# for the different uberScript parameter combinations

# pick a beta (eventually we want the graphs for all three betas next to each other)...
# horizontal axis is the bandwidth
# vertical axis is the RMSE (make sure to include zero as a reference)
# now plot points for each of four categories
  # GCV, SCV, CV, and bandwidths that minimize RMSE for this beta
  # use color and shape to help differentiate the categories

# assuming the working directory is "LWR-Simulations/SpecificationSims"
source("Analysis/FigFunctions.R")

# load up the data file
load("Data/uberScriptOutput20120919.RData")

# grab some colors
require(RColorBrewer)
mypal = brewer.pal(4, "Set1")
myalpha = seq(.5, .1, l = 3) # length equal to the number of error.sds (cause we're going to use transparency to denote the sd)  

# start the pdf file
pdf("Figures/BetaRMSEvsOptimalBandwidths4.pdf")
par(mfcol = c(5, 3)) # 5 x 3 because five sample sizes and three betas
par(oma = c(0, 1, 4.5, 0.5))
par(mar = c(3, 3, 1, 1))

for (my.B2 in 1:4){ # four because there are four different B2sv parameter files
  for (my.B1 in 1:4) { # four because there are four different B1sv parameter files
    for (my.beta in c("B0", "B1", "B2")) {
      for (my.ss in 1:5) { # because we have five sample sizes
        # set plot area - ylim has to include zero and the maximum RMSE score for the three bandwidth selectors for all sample sizes
        max.RMSE = quantile(MetricOutput[ , , my.B1, my.B2, , c("GCV", "SCV", "CV"), paste(my.beta, "RMSE", sep = ".")], .98)
        plot( 1, 1, type = "n",
              xlim = c(0, as.numeric(dimnames(MetricOutput)$ss[my.ss])), xlab = "",
              ylim = c(0, max.RMSE), ylab = "", 
              main = "", 
              axes = F)
        axis(1, labels = c(0, "half", "all"), at = c(0, .5, 1)*as.numeric(dimnames(MetricOutput)$ss[my.ss]))
        axis(2)
        if(my.ss == 1) mtext(my.beta, side = 3, cex = .8, col = brewer.pal(5, "Set1")[5])
        if(my.beta == "B0") mtext(paste("sample size=", dimnames(MetricOutput)$ss[my.ss]), side = 2, line = 2.5, , cex = .8, col = brewer.pal(5, "Set1")[5])

        for (my.error in 1:3){
          #my.ss = my.B1 = my.error = my.B2 = 1
          
          prob = .6 # the proportion of data I want included in the contour plot below
          # GCV
          GCVbw = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("GCV"), "bandwidths"]
          GCVrmse = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("GCV"), paste(my.beta, "RMSE", sep = ".")]
          GCV.contour = peel(GCVbw, GCVrmse, probs = prob)[[1]]
          
          # SCV
          SCVbw = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("SCV"), "bandwidths"]
          SCVrmse = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("SCV"), paste(my.beta, "RMSE", sep = ".")]
          SCV.contour = peel(SCVbw, SCVrmse, probs = prob)[[1]]
          
          # CV
          CVbw = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("CV"), "bandwidths"]
          CVrmse = MetricOutput[my.ss, my.error, my.B1, my.B2, , c("CV"), paste(my.beta, "RMSE", sep = ".")]
          CV.contour = peel(CVbw, CVrmse, probs = prob)[[1]]
          
          # RMSE
          RMSEbw = MetricOutput[my.ss, my.error, my.B1, my.B2, , paste("RMSE", my.beta, sep = "."), "bandwidths"]
          RMSErmse = MetricOutput[my.ss, my.error, my.B1, my.B2, , paste("RMSE", my.beta, sep = "."), paste(my.beta, "RMSE", sep = ".")]
          RMSE.contour = peel(RMSEbw, RMSErmse, probs = prob)[[1]]
          
          mypal.alphas = paste(mypal, sprintf("%X", round(myalpha[my.error]*255)), sep = "")
          
#           polygon(GCVbw[GCV.contour], GCVrmse[GCV.contour],  col = mypal.alphas[1], border = mypal[1])
#           polygon(SCVbw[SCV.contour], SCVrmse[SCV.contour],  col = mypal.alphas[2], border = mypal[2])
#           polygon(CVbw[CV.contour], CVrmse[CV.contour],  col = mypal.alphas[3], border = mypal[3])
#           polygon(RMSEbw[RMSE.contour], RMSErmse[RMSE.contour],  col = mypal.alphas[4], border = mypal[4])
#           
          text(mean(GCVbw), mean(GCVrmse), dimnames(MetricOutput)$error.sd[my.error], col = mypal[1], font = 2, cex = 1.3 )       
          text(mean(SCVbw), mean(SCVrmse), dimnames(MetricOutput)$error.sd[my.error], col = mypal[2], font = 2, cex = 1.3 ) 
          text(mean(CVbw), mean(CVrmse), dimnames(MetricOutput)$error.sd[my.error], col = mypal[3], font = 2, cex = 1.3 ) 
          text(mean(RMSEbw), mean(RMSErmse), dimnames(MetricOutput)$error.sd[my.error], col = mypal[4], font = 2, cex = 1.3 )
          }
        }
      }
    mtext(paste("Bandwidths vs. Beta RMSEs"), 
          outer = TRUE, line = 2, side = 3, font = 2, cex = 1)
    mtext(c("GCV", "SCV", "CV", "RMSE"),
          outer = TRUE, line = 3:0, side = 3, cex = .75, at = .9, adj = 0,
          col = mypal )
    mtext(paste("B1.sv = ", dimnames(MetricOutput)$B1sv[my.B1], "\n", 
                "B2.sv = ", dimnames(MetricOutput)$B2sv[my.B2], sep = ""),
          outer = TRUE, side = 3, line = 1.5, at = .02, adj = 0, col = brewer.pal(5, "Set1")[5], cex = .8)
    }
  }
 
  
dev.off()


