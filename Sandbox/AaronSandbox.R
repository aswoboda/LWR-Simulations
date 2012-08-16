



# playing around with making plots to display optimal bandwidths under GCV vs SCV

pdf("SpecificationSims/Figures/simOutputFigures.pdf")
par(mfrow = c(2, 2))
for (k in 1:4){
  for (i in 1:2) {
    for (j in 1:2) {
      plot(simOutput[k, i, j, 1, , "GCV"], simOutput[k, i, j, 1, , "SCV"],
           xlim = c(0, 500), xlab = "GCV",
           ylim = c(0, 500), ylab = "SCV",
           main = paste("mSCV = ", round(mean(simOutput[k, i, j, 1, , "SCV"]), 0),
                        "mGCV = ", round(mean(simOutput[k, i, j, 1, , "GCV"]), 0)),
           pch = 16, cex = .65,
           col = rgb(red = 0, green = 0, blue = 1, alpha = .05)
      )
    }
  }
}
dev.off()
