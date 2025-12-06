#################     Model Selection   #######################

#When we were first talking about model selection, we used p-values
  #However, this does not necessarily mean the model fits the best

#Basics:
  #goal: compare relative support for different alternative 
  # - but not mutually exclusive - hypotheses

  #Principles: model is well supported if it makes good predictions
    #a model has good predictive ability if the data are highly likely under
    # the model
  #Burnham & Anderson (1998) is definitive source of multimodels


#Likelihood ratio test - Approach of "nested models"
  #F(X) IS SIMPLER MODEL - special case of g(x)
  #g(x) is more complex
      # if f(x) has one variable, g(x) has same variable + at least 1.  If coeff.
      # of additional variable = 0, g(x) = f(x)
  #log likelihoof fation has a chi-square distribution


#AIC: for non-nested models
  #what is expected log-likelihood when applying model to new observations
    #Model will not perform as well on new obs. because it fit as best as possible 
    # to training data, including noise
  #Very flexible models tend to "chase noise" and perform worse with new obs.


#To estimate expected log. likelihood when applying model to new data, adjust 
  # log likelihood of model based on num of params.
  # penalize models that are overly flexible
#AIC = 2*(-Lmax - K),
  #Lmax is neg. log. likelihood of model
  #K is num of params
#Models with lowest AIC give best prediction when applied to new data
#AIC value does not matter, but AICs of alt. models relative to one another is useful
#DeltaAIC - diff. in AIC between candidate model and best model (lowest AIC)



#deltaAIC<2: substantial support, do not exclude
#4 < DeltaAIC < 7: little support: probably exclude
#DeltaAIC > 10: def. no support, def. exclude



# If multiple models:
  #if nested, take simpler
  #Evaluate whether models predict data well
  #there will always be a better model w/in set of candidate
  #multimodel


#logLik.lm() - extract log-likelihood of a model

install.packages("lmtest")
library(lmtest)      #we want lrtest() for nested models
mtcars 
#fit full model

model_full <- glm(vs ~ mpg + disp + hp, data = mtcars, family = binomial)

#fit reduced model

model_reduced <- glm(vs~ mpg + disp, data = mtcars, family = binomial)

lrtest(model_full, model_reduced)        #tests models v. each other (nested)
      #IF significant, model needs additional parameter
lrtest(model_reduced, model_full)         #swapping models for shits and giggles
#
