####READ ME####
#The purpose of this script is to extract the error from the zinb movel
#in this case I am going to use McFadden's pseudo R2.
####libraries####
library(glmmTMB) #zero inflated
library(DescTools) #logLik (McFadden’s R2)
library(purrr)

####load models and data####
zinb_5 <- readRDS("Data/zinb_models.RData")
#model data frame list 
alldf <- readRDS("Data/df_list.RData")

####test McFadden’s R2 with one model####
#take one model from my model list
example <- zinb_5[["08317400_100"]]
#calculate the log-likelihood of the fitted model
loglik_model <- logLik(example)

#calculate the null model for all data frames
null_all <- lapply(alldf, function(df) {
  null_model <- glmmTMB(Sum_days_rm_dry ~ 1, data = df, ziformula = ~1, family = nbinom2)
})

#take one model from null model list
example_null <- null_all[["08317400_100"]]
#calculate the log-likelihood of the fitted null model
loglik_model_null <- logLik(example_null)

#compute McFadden's R-squared as 1 minus the ratio of the log-likelihood of the fitted model to the log-likelihood of the null model
MFR2_example <- 1 - (logLik(example)/logLik(example_null))
MFR2_example

####compute McFadden for all models####
#remove ks from list to run logLik function
# Use map() from purrr to extract the first element from each list
#zinb <- map(zinb_5, 1)

#run logLik for model list
loglik_zinb <- list()    
for (i in seq_along(zinb)) {
  output <- logLik(zinb[[i]])
  loglik_zinb[[i]] <- output
}

#run logLik for null model list
loglik_null <- list()    
for (i in seq_along(null_all)) {
  output <- logLik(null_all[[i]])
  loglik_null[[i]] <- output
}

#compute McFadden's R-squared for all models
loglik_list <- list()  
for (name in names(zinb)) {
  output <- 1 - (logLik(zinb_5[[name]])/logLik(null_all[[name]]))
  loglik_list[[name]] <- output
}

#save list
saveRDS(loglik_list, file="Data/loglik_list.RData")
