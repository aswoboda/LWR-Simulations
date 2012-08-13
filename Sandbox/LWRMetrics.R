# Matrix for each parameter/metric (each B, st.e, leverage, dep.var) 
# row represents each observation, column represents each k


# For correlation, need to pull the Bs for each observation and level of K and match with trueB0,B1,B2


source("SpecificationSims/SimFunctions.R")
new.output = Reorganizer(output)
##### IMPORTANT

# Reorganize data, 1 matrix for each B and metric
# Correlations on Bs, dep. values
# T-tests, need global regression vs LWR, see if there's any statisical difference between methods
# B residuals

# Correlations: For 3 Bs and the dep. var:
# Could probably make this less repetitive. Create a function that correlates all Bs and dep.var
# and then stores them in matrices. 
# As of now, beta0s are constant, and so cannot calculate correlations
# beta0.cor.results = cor(new.output$beta0hats, Data$trueB0) #B0  #Can't use a function?
beta1.cor.results = cor(new.output$beta1hats, Data$trueB1) #B1
beta2.cor.results = cor(new.output$beta2hats, Data$trueB2) #B2
dep.var.cor.results = cor(new.output$yhats), Data$dep.var) #dependent variable


# Residuals: For B0, B1, B2
# Want to meausure residual of each B for each k for each observation 
# against the trueB for that observation. 
beta.Residual.Calc = function(betahats, truebetas) {
  sum(([ i, ] - Data$trueB1[ , i])^2)
}



# T-tests
lm(dep.var ~ indep.var1 + indep.var2, data = Data.Frame)
# Want to test for significance the difference between each betahat and its corresponding truebeta.
kvector <- seq(minimumk, sample.size - 1, up.by)
if (sample.size %% up.by != 1)  kvector = c(kvector, sample.size - 1)

t.percent = c() # Flexible length vector

t.obs = ((new.output[["beta0hats"]] - Data$trueB0)/new.output[["ses0"]])
critical.ts = qt(.975, kvector-2) # .975 = our significance level, kvector-2 tells us our DoF, K+observation-3 betas
for (j in 1: length(critical.ts)) {
t.percent[j] = sum(t.obs[ , j] > critical.ts[j])/dim(t.obs)[1]
}
     
# Calculate critical t for each k
# Find % for each k > critical t
