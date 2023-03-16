# March 2023 
# by Madelaine Anderson and Erica Zaja 
# Function to extract model outputs from brms + code to make a beautiful output table.

# PART 1 -----
# Function to extract model info from brms model and add to dataframe that can then be easily formatted. 

# With random effects (currently named random but you could change that)
# change the RANDOM in sum$random$RANDOM to whatever your random effect is called e.g. "year"
# note: you can probably easily modify to include mulitple random effects but haven't tried yet  
# also haven't tried changing to include nested but should be a simple addition 

model_summ <- function(x) {
  sum = summary(x)
  fixed = sum$fixed
  sigma = sum$spec_pars
  random = sum$random$random
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
mod <- brms::brm(X~Y+(1|RANDOM)...)
mod_results <- model_summ(mod) # returns data frame with estimates, errors, CIs, tail ESS, bulk ESS and nobs aka number of observations 

# PART 2 ------

# Creating a table with kable! 
library(kableExtra)
library(knitr)
webshot::install_phantomjs()

# optional: change row names of the dataframe of extracted model results
rownames(mod_results) <- c("", "", "", "")

# optional: reorder columns 
mod_results <- mod_results %>%
  relocate("col.name", .before = "col.name")

# optional: merge extracted outputs of multiple models
mod_results_merge <- rbind(,,,)
  
# if you have the same row names multiple times, the table we will use later will 
# not allow you to use them, and will automatically add a "1","2", "3" to differentiate them.
# Adding spaces before/after each name will trick this system. Example below.  
# "Intercept", "Southern Garden", "Sample age" are examples of row names
rownames(mod_results) <- c("Intercept", "Southern Garden", "Sample age", 
                                       "Sigma", " Intercept", " Southern Garden", " Sample age", 
                                       " Sigma", "Intercept ", "Southern Garden ", "Sample age ", 
                                       "Sigma ")

# making sure Rhat keeps the .00 decimals 
mod_results$Rhat <- as.character(formatC(mod_results$Rhat, digits = 2, format = 'f')) #new character variable with format specification

# creating table
kable_name <- mod_results %>% 
  kbl(caption="Table.xxx ", 
      col.names = c("", "", "" ), # give the column names you want making sure you have one name per column!
      digits=2, align = "c") %>%  # specify number of significant digits, align numbers at the centre (can also align "l" left/ "r" right)
  kable_classic(full_width=FALSE, html_font="Cambria") # can change fonts

# optional: making specific column text in cursive
column_spec(kable_name, 2, width = NULL, bold = FALSE, italic = TRUE) # 2 is my example column number 

# saving table: from here https://rdrr.io/cran/kableExtra/man/save_kable.html 
save_kable(kable_name,file = ".pdf", # or .png, or .jpeg, save in your working directory
           bs_theme = "simplex",
           self_contained = TRUE,
           extra_dependencies = NULL,
           latex_header_includes = NULL,
           keep_tex = FALSE,
           density = 300)

