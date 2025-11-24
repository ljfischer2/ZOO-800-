# Linear model:
  # y = B0 + B1x1 + B2x2... + Error
#Estimates of Betas may still be OK
  #Best unbiased estimator for a linear regression is OLS estimator, provided:
    #Uncorrelated
    #Mean 0
    #Homoscedastic
    #Not necessarily normal

#If not normal: 
#Transformation?
  #log transformation for skewed data
  #arcsin transformation for proportion data
    #arcsin isn't useful?
      #coefficients aren't interpretable in terms of original response variable
      #power is often lower than that of appropriate GLM
      #mean-variance relationship is not always preserved in transformations

#Typical types of non-normal
  #Counts
  #Proportions
  #Categorical

#Each data type has typical distribution
  #Counts are Poisson if density is constant, independent
  #Discrete probability distribution
    #integer from 0 to INF
  #single parameter: mean = var = lambda

## of sucesses in n independent trials follow binomial

#glm function in R
  #glm(formula, family = familytype(link = linkfunction), data = )
  #Formula: y~x1 + x2... OR y~x1*x2...
  #Family-default = Gaussian
  #Link - each family has a default link
#Data for binomial- three options for Y:
  #two column matrix of sucesses and failures
  #factor, where first level encountered is "failure)

#Link functions
  #link functions link data to linear predictor
  #Binomial defined by parameter p
  #logit: n = log(p/(1-p))
    #p/(1-p) = odds 
    #So: logit is the log-odds

