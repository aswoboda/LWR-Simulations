# empty sandbox
# 
# require(RColorBrewer)
# mypal = brewer.pal(4, "Set2")
# # > mypal
# # [1] "#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3"
# # > col2rgb(mypal)
# # [,1] [,2] [,3] [,4]
# # red    102  252  141  231
# # green  194  141  160  138
# # blue   165   98  203  195
# 
# # col2rgb(mypal) -> rgbpal
# 
# 
# myalpha = seq(.4, .8, l = 3)  
# 
# plot(c(1, 4), c(1, 3), type = "n", axes = F, xlab = "", ylab = "")
# 
# for (i in 1:3) {
# 
#   cols = paste(mypal, sprintf("%X", round(myalpha[i]*255)), sep = "")
#   points(1:4, rep(i, 4), pch = 16, cex = 5, 
#          col = cols)
# }



# Working on the code to make a grid of figures with outside labels for the rows and columns
#data(cars)

df = layout( matrix(c(0, rep(17, 4),
                      18, 1:4,
                      18, 5:8,
                      18, 9:12,
                      18, 13:16), 5, 5, byrow = T),
             widths = c(.6, rep(1, 4)),
             heights = c(.6, rep(1, 4)))

layout.show(df)

par(oma = c(0, 0, 4, 0))
par(mar = c(2, 2, 2, 2))
for (i in 1:16) {
  plot(mtcars$wt, mtcars$mpg, axes = F)
  box()
}

# Now work on the column labels
par(mar = c(0, 0, 0, 0))
spots = seq(.125, .875, l = 4)
plot(1, xaxs="i", xlim = c(0, 1), yaxs = "i", ylim = c(0, 1), type = "n", axes = F)
axis(1, line = -2, at = spots, 
     labels = F, col = "red")
text(spots, rep(0, 4), c("none", "some", "more", "most"), 
     col = "red", pos = 3, cex = 1.3)
text(.5, .5, "Degree of Spatial Variation in B1", 
     col = "red", cex = 1.5, font = 2)
#points((0:100)/100, rep(0, 101), col = c("red", rep("black", 9)))

# Now work on the row labels
plot(1, xaxs="i", xlim = c(0, 1), yaxs = "i", ylim = c(0, 1), type = "n", axes = F)
axis(4, line = -2, at = spots, 
     labels = F, col = "red")
text(rep(.9, 4), spots, c("none", "some", "more", "most"), 
     col = "red", cex = 1.3, srt = 90)
text(.5, .5, "Degree of Spatial Variation in B2", 
     col = "red", cex = 1.5, font = 2, srt = 90)

# Title
mtext("Main Title" , side = 3, outer= T)