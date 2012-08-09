# LWR for all Ks - specify a model as a parameter
LWR = function(i, Data.Frame, model = "dep.var ~ x1 + x2") {
  sample.size = dim(Data.Frame)[1]
  
  
  tempyhat = c(1:numK)
  tempbetahat = c(1:numK)
  tempstd = c(1:numK)
  tempSi = c(1:numK)
  Di = D[[i]]
  model = "dep.var ~ x1"
  
  for (j in 1:numK) { # j is the position of our k in the kvector
    k <- kvector[j]
    # calculate the bisquare weights
    threshold = sort(Di)[k+1] # b is the threshold distance (distance to the k+1 th nearest observation to obs i)
    
    weights = (1-(Di/b)^2)^2
    weights[Di>b] = 0
    lmreg = lm(model, weights = weights)
    for (i in 1:sample.size {
      D[[i]]<-((north-north[i])^2+(east-east[i])^2)^.5
    }
     
  

  
  
  
DataGen = function(sample.size, error.sd, B1.spatial.var, B2.spatial.var) {
  # Set sample size
  n = sample.size
  
  # Give observations coordinates 
  east = runif(sample.size)*10 
  north = runif(sample.size)*10
  indep.var1 = runif(sample.size)*10 # random x1's
  indep.var2 = runif(sample.size)*10 # random x2's
  
  trueB1 = B1.spatial.var*north/4 + 1 - 5*B1.spatial.var
  trueB2 = B2.spatial.var*5 + 1 - B2.spatial.var*east
  
  
  error=rnorm(sample.size, 0, error.sd)
  
  dep.var = x1*trueB1+x2*trueB2 + error
  
  data.frame(dep.var, north, east, indep.var1, indep.var2, trueB1, trueB2)
}


