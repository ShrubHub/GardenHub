# March 2023 
# by Madelaine Anderson and Erica Zaja 
# Function to extract model info from brms model and add to dataframe that can then be easily formatted 

# with random effects (currently named random but you could change that)

model_summ <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$year
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  random$nobs <- obs
  sigma$nobs <- obs
  
  row.names(random)[row.names(random) == "sd(Intercept)"] <- "random"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge together
}

# without random effects

model_summ_2 <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  obs = sum$nobs
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  sigma$effect <- "residual"
  fixed$nobs <- obs  # add column with number of observations
  sigma$nobs <- obs
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, sigma))  # merge together
}

# to use
# example model
mod <- brms::brm(X~Y+(1|year)...)
mod_results <- model_summ(mod) # returns data frame with estimates, errors, CIs, tail ESS, bulk ESS and nobs aka number of observations 


