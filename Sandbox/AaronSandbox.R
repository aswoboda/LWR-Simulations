# empty sandbox

require(RColorBrewer)
mypal = brewer.pal(4, "Set2")
# > mypal
# [1] "#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3"
# > col2rgb(mypal)
# [,1] [,2] [,3] [,4]
# red    102  252  141  231
# green  194  141  160  138
# blue   165   98  203  195

# col2rgb(mypal) -> rgbpal


myalpha = seq(.4, .8, l = 3)  

plot(c(1, 4), c(1, 3), type = "n", axes = F, xlab = "", ylab = "")

for (i in 1:3) {

  cols = paste(mypal, sprintf("%X", round(myalpha[i]*255)), sep = "")
  points(1:4, rep(i, 4), pch = 16, cex = 5, 
         col = cols)
}

