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


# Residuals: For beta0/1/2
# beta.Residual.calc is a function defined in SimFunctions and can be sourced
beta.Residual.Calc(new.output[["beta0hats"]], Data$trueB0)
beta.Residual.Calc(new.output[["beta1hats"]], Data$trueB1)
beta.Residual.Calc(new.output[["beta2hats"]], Data$trueB2)

# Now calculate for each beta the % of t-tests with values > critical t, for each k.
beta.ttest(new.output[["beta0hats"]], new.output[["ses0"]], Data$trueB0)
beta.ttest(new.output[["beta1hats"]], new.output[["ses1"]], Data$trueB1)
beta.ttest(new.output[["beta2hats"]], new.output[["ses2"]], Data$trueB2)
     
# Generalized Cross-validation score

GCV(new.output[["leverages"]], new.output[["yhats"]], Data$dep.var)
         ##Define GCV score

# Standardized CV a la Paez 2007
standardized.CV = function(Data$dep.var, new.output[["yhats.without"]]) 


