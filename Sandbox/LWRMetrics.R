# Matrix for each parameter/metric (each B, st.e, leverage, dep.var) 
# row represents each observation, column represents each k


# For correlation, need to pull the Bs for each observation and level of K and match with trueB0,B1,B2


source("SpecificationSims/SimFunctions.R")

##### IMPORTANT

# Reorganize data, 1 matrix for each B and metric
# Correlations on Bs, dep. values
# T-tests, need global regression vs LWR, see if there's any statisical difference between methods
# B residuals

# Correlations: For 3 Bs and the dep. var:
# Could probably make this less repetitive. Create a function that correlates all Bs and dep.var
# and then stores them in matrices. 

beta0.cor.results = cor(Reorganizer(output)$beta0hats, Data$trueB0) #B0  #Can't use a function?
beta1.cor.results = cor(Reorganizer(output)$beta1hats, Data$trueB1) #B1
beta2.cor.results = cor(Reorganizer(output)$beta2hats, Data$trueB2) #B2
dep.var.cor.results = cor(Reorganizer(output)$yhats), Data$dep.var) #dependent variable


# Residuals: For B0, B1, B2
# Want to meausure residual of each B for each k for each observation 
# against the trueB for that observation. 
beta.Residual.Calc = function(betahats, truebetas) {
  sum((betahats[ i, ] - truebetas[ , i])^2)
}



# T-tests
lm(dep.var ~ indep.var1 + indep.var2, data = Data.Frame)
# Want to test for significance the difference between each betahat and its corresponding truebeta.
t.test(Reorganizer(output)$beta0hats, y=Data$trueb0)