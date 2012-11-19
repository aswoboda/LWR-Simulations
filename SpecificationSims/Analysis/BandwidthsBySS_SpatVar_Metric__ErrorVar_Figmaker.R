# This script makes a series of plots showing the bandwidth beanplots by Sample Size, Spatial Variation, and Metric

# A page for each sample size
# a grid of 16 plots showing the 16 different spatial variations in the coefficients
# a beanplot of the bandwidth distributions for the four different metrics

# I'm basically just grabbing the code from the BandwidthResults.Rnw file and adding a loop for the sample sizes

# Load the data
setwd("~/Documents/Research/LWR-Simulations/SpecificationSims/Figures")
load("../Data/uberScriptOutput20120919.RData")

# set some parameters

require(RColorBrewer)
require(beanplot)
mypal = brewer.pal(4, "Set2")

myss = c("50", "100", "200", "500", "1000")
mymetrics = c("CV", "GCV", "SCV", "AICc")
bws = c(2.5, 2.5, 5, 5, 10)

# open the pdf device
pdf("BandwidthsBySS_SpatVar_Metric_Error.pdf", family = "Palatino", width = 6.5, height = 8)

df = layout( matrix(c(0, rep(17, 4),
                      18, 1:4,
                      18, 5:8,
                      18, 9:12,
                      18, 13:16), 5, 5, byrow = T),
             widths = c(.6, rep(1, 4)),
             heights = c(.6, rep(1, 4)))
myssi = myss[1]
for (myssi in myss) {

par(oma = c(0, 0, 1, 3.5))
par(mar = c(1, 1, 1, 1))

my.B1 = my.B2 = 1
for (my.B2 in 1:4){ # four because there are four different B2sv parameter files
  for (my.B1 in 1:4) { # four because there are four different B1sv parameter files
    # now make a plot for the given Bsv1 and Bsv2
    plot(c(0.5, 15.5), c(0, as.numeric(myssi)), type = "n", 
         main = "", ylab = "", xlab = "", axes = F)

    myxs = matrix(c(1:3, 5:7, 9:11, 13:15), ncol = 4, nrow = 3)
    
    for (metric in 1:4) { #loop through the four metrics
      for (errorVar in 1:3) { #loop through the thres error variances
        # grab the relevant bandwidths
        mybandwidths = MetricOutput[myssi, errorVar, my.B1, my.B2, , mymetrics[metric], "bandwidths"]
        # plot the middle 90% of the bandwidths and the mean bandwidth
            # x = 1:3, 4:7, 9:11, 13:15
          myx = myxs[errorVar, metric]
            # middle 90% = 
          bandwidthRange = quantile(mybandwidths, c(.05, .95))
            # mean
          bandwidthMean = mean(mybandwidths)
        lines(rep(myx, 2), bandwidthRange, col = mypal[metric], lwd = 2)
        points(myx, bandwidthMean, pch = 16, col = "white", cex = 1.5)
        text(myx, bandwidthMean, c("S", "M", "L")[errorVar], col = mypal[metric], cex = .8)
      }
    }
    
    if(my.B1 == 1) axis(2, las = 1) #, at = seq(0, as.numeric(myssi), l = 5)
    if(my.B1 == 4) axis(4, las = 1)
  }
}

# Now work on the column labels
par(mar = c(0, 0, 0, 0))
spots = seq(.125, .875, l = 4)
plot(1, xaxs="i", xlim = c(0, 1), yaxs = "i", ylim = c(0, 1), type = "n", axes = F)
axis(1, line = -2, at = spots, 
     labels = F, col = "red")
text(spots, rep(0, 4), c("none", "some", "more", "most"), 
     col = "red", pos = 3, cex = 1.3)
text(.5, .4, "Degree of Spatial Variation in $\\beta_1$", 
     col = "red", cex = 1.2)
#points((0:100)/100, rep(0, 101), col = c("red", rep("black", 9)))

# Now work on the row labels
plot(1, xaxs="i", xlim = c(0, 1), yaxs = "i", ylim = c(0, 1), type = "n", axes = F)
axis(4, line = -3.5, at = spots, 
     labels = F, col = "red")
text(rep(.65, 4), spots, c("most", "more", "some", "none"), 
     col = "red", cex = 1.3, srt = 90)
text(.2, .5, "Degree of Spatial Variation in $\\beta_2$", 
     col = "red", cex = 1.2, srt = 90)

# Title and Legend
mtext(paste("Bandwidth Distributions by Degree of Spatial Variation, Metric, and Error Variance"), 
      outer = TRUE, line = -1.5, side = 3, font = 2, cex = 1, at = .53)
mtext(c("Metric =", "CV", "GCV", "SCV", "AICc"), outer = TRUE, line = -21.5, side = 3, 
      cex = .8, at = c(.28, .37, .405, .46, .508), adj = 0, 
      col = c("black", mypal), font = 2)
mtext(paste0("(Sample Size = ", as.numeric(myssi),")"), outer = TRUE, side = 3, 
      line = -3, at = .84, cex = .95, adj = 0)
mtext("# of obs. in \nbandwidth", side = 3, outer = TRUE, line = -8.5, at = .07, adj = 0, cex = .7)
}
dev.off()