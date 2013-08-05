load("SpecificationSims/Data/uberScriptOutput20120919.RData")

require(RColorBrewer)
require(beanplot)
mypal = brewer.pal(4, "Set2")

# set some figure margin parameters

my.B1 = my.B2 = 1

pdf("SpecificationSims/Figures/OptimalBandwidthBeanplotsByCVSpatialVar.pdf", 
    height = 8, width = 8, family="Palatino")
df = layout( matrix(c(0, rep(17, 4),
                      18, 1:4,
                      18, 5:8,
                      18, 9:12,
                      18, 13:16), 5, 5, byrow = T),
             widths = c(.6, rep(1, 4)),
             heights = c(.6, rep(1, 4)))

#layout.show(df)

myssi = "1000"
myss = c("50", "100", "200", "500", "1000")
mymetrics = c("CV", "GCV", "SCV", "AICc")
bws = c(2.5, 2.5, 5, 5, 10)

par(oma = c(0, 0, 0, 2.5))
par(mar = c(.5, .5, .5, .5))

for (my.B2 in 1:4){ # four because there are four different B2sv parameter files
  for (my.B1 in 1:4) { # four because there are four different B1sv parameter files
    # now make a beanplot for the given Bsv1 and Bsv2
    beanplot(MetricOutput[myssi, , my.B1, my.B2, , mymetrics[1], "bandwidths"],
             MetricOutput[myssi, , my.B1, my.B2, , mymetrics[2], "bandwidths"],
             MetricOutput[myssi, , my.B1, my.B2, , mymetrics[3], "bandwidths"],
             MetricOutput[myssi, , my.B1, my.B2, , mymetrics[4], "bandwidths"],
             what = c(0, 1, 1, 0) , 
             log = "",
             bw = bws[which(myss == myssi)], 
             cutmin = 5, 
             cutmax = as.numeric(myssi),
             ylim = c(0, as.numeric(myssi)),
             #names = mymetrics,
             axes = FALSE,
             main = "",
             ylab = "",
             col = list(col = mypal[1], col = mypal[2], col = mypal[3], col = mypal[4]))
    if(my.B1 == 1) axis(2, las = 1)
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
text(.5, .4, expression(paste("Degree of Spatial Variation in ", beta[1])), 
     col = "red", cex = 1.5, font = 2)
#points((0:100)/100, rep(0, 101), col = c("red", rep("black", 9)))

# Now work on the row labels
plot(1, xaxs="i", xlim = c(0, 1), yaxs = "i", ylim = c(0, 1), type = "n", axes = F)
axis(4, line = -5, at = spots, 
     labels = F, col = "red")
text(rep(.5, 4), spots, c("most", "more", "some", "none"), 
     col = "red", cex = 1.3, srt = 90)
text(.2, .5, expression(paste("Degree of Spatial Variation in ", beta[2])), 
     col = "red", cex = 1.5, srt = 90)

# Title and Legend
mtext(paste("Bandwidth Distributions by Metric and Degree of Spatial Variation"), 
      outer = TRUE, line = -2.5, side = 3, font = 2, cex = 1.5, at = .52)
mtext(c("Metric =", "CV", "GCV", "SCV", "AICc"), outer = TRUE, line = -21, side = 3, 
      cex = .8, at = c(.3, .375, .41, .46, .508), adj = 0, 
      col = c("black", mypal), font = 2) 

dev.off()