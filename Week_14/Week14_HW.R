#HW 14 Lucas Fischer



############ Objective 1 ###########


data <- read.csv('Week_14/SquirrelsEatNuts.csv')

model_full <- glm(NutsConsumedPerDay ~ BodyMassPounds + species, data = data)
model_reduced <- glm(NutsConsumedPerDay ~ BodyMassPounds, data = data)

M_F_NLL <- logLik(model_full)
M_R_NLL <- logLik(model_reduced)

logLik(model_full)    #-196.4974
logLik(model_reduced) #-196.4982
  #the two models have nearly identical log likelihood

lrtest(model_full, model_reduced) #Not gonna lie, I don't need to know the
  # results of this to tell you it doesn't matter
  #The lr test says there is no difference in the two models, so we opt for 
  # the simpler model

######## Objective 2 ###################

#create each model
model_full <- glm(NutsConsumedPerDay ~ BodyMassPounds * species, data = data) #df5
model_int <- glm(NutsConsumedPerDay ~ BodyMassPounds + species, data = data)  #df4
model_reduced <- glm(NutsConsumedPerDay ~ BodyMassPounds, data = data)        #df3
model_none <- glm(NutsConsumedPerDay ~ 1, data = data)                        #df2
modellist <- c("model_full","model_int","model_reduced","model_none")

M_F_LL <- logLik(model_full)
M_I_LL <- logLik(model_int)
M_R_LL <- logLik(model_reduced)
M_N_LL <- logLik(model_none)

M_F_AIC = 2*(-M_F_LL - 5)
M_I_AIC = 2*(-M_I_LL - 4)
M_R_AIC = 2*(-M_R_LL - 3)
M_N_AIC = 2*(-M_N_LL - 2)

AIC_tbl <- data.frame(Model = modellist,
                      NLL = c(M_F_LL,M_I_LL, M_R_LL, M_N_LL),
                      AIC = c(M_F_AIC, M_I_AIC, M_R_AIC, M_N_AIC))
print(AIC_tbl)        #Our full model has the Lowest AIC
#This indicates that our full model is the most appropriate when applying the 
# model to new data.  We should use this model moving forward for modelling 
# nuts consumed per day
