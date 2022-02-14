## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----installation, eval = F---------------------------------------------------
#  install.packages("riskCommunicator")

## ----setup--------------------------------------------------------------------
library(riskCommunicator)
library(tidyverse)
library(printr)

## ---- printr.help.sections = c('usage','arguments')---------------------------
?gComp

## -----------------------------------------------------------------------------
data(cvdd)

## ----binary_outcome, paged.print = FALSE--------------------------------------
## Specify the regression formula
cvdd.formula <- cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP

## For reproducibility, we should always set the seed since the g-computation uses  
## random resampling of the data to calculate confidence intervals and random  
## sampling of the distribution when predicting outcomes.
set.seed(1298)

## Call the gComp function
binary.res <- gComp(data = cvdd, 
                    formula = cvdd.formula, 
                    outcome.type = "binary", 
                    R = 1000)

binary.res

## ----gComp_class_explaination-------------------------------------------------
class(binary.res)

## The names of the different items in the list: 
names(binary.res)

## Sample size of the original data:
binary.res$n 

## Contrast being compared in the analysis:
binary.res$contrast

## ----rate_outcome, paged.print = FALSE----------------------------------------
## Modify the dataset to change the variable cvd_dth from a factor to a numeric variable  
## since the outcome for Poisson regression must be numeric.
cvdd.t <- cvdd %>%
  dplyr::mutate(cvd_dth = as.numeric(as.character(cvd_dth)),
                timeout = as.numeric(timeout))

set.seed(6534)

rate.res <- gComp(data = cvdd.t, 
                  Y = "cvd_dth", 
                  X = "DIABETES", 
                  Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                  outcome.type = "rate", 
                  rate.multiplier = 365.25*100, 
                  offset = "timeout", 
                  R = 1000)

rate.res

## ----rate_outcome_subgroup, paged.print = FALSE-------------------------------
## Modify the dataset to change the variable cvd_dth from a factor to a numeric variable  
## since the outcome for Poisson regression must be numeric.

set.seed(6534)

rate.res.subgroup <- gComp(data = cvdd.t, 
                           Y = "cvd_dth", 
                           X = "DIABETES", 
                           Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                           subgroup = "SEX", 
                           outcome.type = "rate", 
                           rate.multiplier = 365.25*100, 
                           offset = "timeout", 
                           R = 1000)

rate.res.subgroup

## ----binary_outcome_plot, fig.width = 12, fig.height = 9, out.width = "100%"----
plot(binary.res)

## -----------------------------------------------------------------------------
sessionInfo()

