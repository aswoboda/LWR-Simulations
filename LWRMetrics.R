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

beta0.cor.results = cor(Reorganizer(output)[1], Data$trueB0) #B0
beta1.cor.results = cor(Reorganizer(output)[2], Data$trueB1) #B1
beta2.cor.results = cor(Reorganizer(output)[3], Data$trueB2) #B2
dep.var.cor.results = cor(Reorganizer(output)[7]), Data$dep.var) #dependent variable

# Residuals: For B0, B1, B2
betaResidual.Calc = function(i, betas) {
  sum((betas[, 1] - betas[, i])^2)
}

# T-tests
lm(dep.var ~ indep.var1 + indep.var2, data = Data.Frame)
t-test()