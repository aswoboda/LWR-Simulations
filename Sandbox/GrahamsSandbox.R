testmat <- matrix(1:16, nrow = 4, ncol = 4)
testmat

#returns the index of the minimum.  You can find any ranking value by replacing min with that value
#such as sort(testmat)[2] to find the second lowest
which(min(testmat) == testmat, arr.ind = T)