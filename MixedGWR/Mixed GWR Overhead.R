
#setwd("~/Desktop") #sets working directory (and the location of pdfs created by this script)
#Graham: wd is in dropbox

install.packages(c("spam", "fields", "abc"))
library(spam)
library(fields)
library(abc)

require(fields) # make sure you have this package installed, otherwise the distance calcs won't work
# Mixed GWR as desribed on pp 67-8 in the GWR book by Fotheringham, et al.

# n observations
# a variables believed to have stationary effect over space in a matrix Xa
# b variables belived to have non-stationary effect over space in a matrix Xb

# for each column of Xa:
# regress the column against Xb using basic GWR
# compute the residuals from the above regression
# regress y against Xb using basic GWR
# compute the residuals from the above regression
# regress the y residuals against the Xa residuals using OLS -> this yields ahat, the stationary coefficients
# subtract Xa*ahat from y. regress this against Xb using basic GWR to obtain the geographically varying coefficients

# note that if there are ka a-group variables, then using this algorithm requires running basic GWR ka + 2 times

####
#### Some functions
####
bisquare = function(d, dk) { # takes a vector of distances and a threshold distance
  weights = (1 - (d/dk)^2)^2 # for distances less than dk, weights are here
  weights[d>dk] = 0 # for distances farther away than dk, weights are zero
  weights 
}

##### Graham: I think you can just do sum for summing y - yhat, you dont need to make them matrixes first if they're just vectors
GCV = function(y, yhat, levs, nonstationary = 0) {
  sample.size = length(y)
  v1 = nonstationary + colSums(matrix(levs))
  SE <- colSums((matrix(y) - matrix(yhat))^2)
  gcv <- sample.size*SE/(sample.size - v1)^2
  gcv
}
