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

## ----load_other_data, eval = FALSE--------------------------------------------
#  mydata <- read.csv("C:/your/file/path/yourdata.csv")

## ----dataset------------------------------------------------------------------
data(cvdd)

## ----variable_check-----------------------------------------------------------
cvdd$educ <- as.factor(cvdd$educ)
#educ is now a factor with 4 levels

str(cvdd$educ)

## ----error = TRUE-------------------------------------------------------------
cvdd.break <- cvdd %>% 
  mutate(PREVHYP = as.character(PREVHYP))
  
binary.res.break <- gComp(data = cvdd.break, 
                          Y = "cvd_dth", 
                          X = "DIABETES", 
                          Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                          outcome.type = "binary", 
                          R = 200)

## ----factor_check-------------------------------------------------------------
str(cvdd$educ)

cvdd$educ <- factor(cvdd$educ,levels = c("4","1","2","3"))
#Category 4 is now the referent

str(cvdd$educ)

## -----------------------------------------------------------------------------
cvdd %>%
 select(everything()) %>%
 summarise_all(list(~sum(is.na(.))))

## ----binary_outcome, paged.print = FALSE--------------------------------------

## Specify the regression formula
cvdd.formula <- cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP

## For reproducibility, we should always set the seed since the g-computation uses random resampling of the data to calculate confidence intervals and random sampling of the distribution when predicting outcomes
set.seed(1298)

## Call the gComp function
binary.res <- gComp(data = cvdd, 
                    formula = cvdd.formula, 
                    outcome.type = "binary", 
                    R = 200)

## ----binary_outcome_noFormula-------------------------------------------------
set.seed(1298)

binary.res.alt <- gComp(data = cvdd, 
                        Y = "cvd_dth", 
                        X = "DIABETES", 
                        Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                        outcome.type = "binary", 
                        R = 200)

## ----binary_outcome_defaults--------------------------------------------------
set.seed(1298)

binary.res.alt2 <- gComp(data = cvdd, 
                         formula = cvdd.formula, 
                         outcome.type = "binary", 
                         R = 200, 
                         Y = NULL, 
                         X = NULL, 
                         Z = NULL, 
                         subgroup = NULL, 
                         offset = NULL, 
                         rate.multiplier = 1, 
                         clusterID = NULL, 
                         parallel = "no", 
                         ncpus = 1)

## ----binary_results_check, paged.print = FALSE--------------------------------
binary.res
print(binary.res)

## ----gComp_class_explaination-------------------------------------------------
class(binary.res)
# The names of the different items in the list 
names(binary.res)

# To see the sample size of the original data:
binary.res$n 

# To see the contrast being compared in the analysis:
binary.res$contrast

## ----binary_outcome_subgroup, paged.print = FALSE-----------------------------
set.seed(1298)

binary.res.subgroup <- gComp(data = cvdd, 
                             Y = "cvd_dth", 
                             X = "DIABETES", 
                             Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                             subgroup = "SEX", 
                             outcome.type = "binary", 
                             R = 200)

binary.res.subgroup

## ----categorical_exposure, paged.print = FALSE--------------------------------
#number and percent of subjects in each BMI category 
table(cvdd$bmicat)
prop.table(table(cvdd$bmicat))*100

set.seed(345)
catExp.res <- gComp(data = cvdd, 
                    Y = "cvd_dth", 
                    X = "bmicat", 
                    Z = c("AGE", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), 
                    outcome.type = "binary", 
                    R = 200)

catExp.res


## ----continuous_exposure, paged.print = FALSE---------------------------------
set.seed(4528)
contExp.res <- gComp(data = cvdd, 
                     Y = "cvd_dth", 
                     X = "AGE", 
                     Z = c("BMI", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), 
                     outcome.type = "binary", 
                     exposure.scalar = 10, 
                     R = 200)

contExp.res


## ----change_dataset_cvd_dth_to_numeric----------------------------------------
cvdd.t <- cvdd %>%
  dplyr::mutate(cvd_dth = as.numeric(as.character(cvd_dth)),
                timeout = as.numeric(timeout))

## ----rate_outcome, paged.print = FALSE----------------------------------------
set.seed(6534)

rate.res <- gComp(data = cvdd.t, 
                  Y = "cvd_dth", 
                  X = "DIABETES", 
                  Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                  outcome.type = "rate", 
                  rate.multiplier = 365.25*100, 
                  offset = "timeout", 
                  R = 200)

rate.res

## ---- paged.print = FALSE-----------------------------------------------------
## Specify the regression formula
cvdd.formula <- cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP

set.seed(6534)
## Call the gComp function
rate.res.alt <- gComp(data = cvdd.t, 
                      formula = cvdd.formula, 
                      outcome.type = "rate", 
                      rate.multiplier = (365.25*100), 
                      offset = "timeout", 
                      R = 200)
rate.res.alt


## ----continuous_outcome, paged.print = FALSE----------------------------------
set.seed(9385)

cont.res <- gComp(data = cvdd, 
                  Y = "glucoseyear6", 
                  X = "DIABETES", 
                  Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                  outcome.type = "continuous", 
                  R = 200)

cont.res

## ----count_outcome, paged.print = FALSE---------------------------------------
set.seed(7295)

count.formula <- "nhosp ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP"

count.res <- gComp(data = cvdd, 
                   formula = count.formula, 
                   outcome.type = "count", 
                   R = 200)

count.res

## ----catExp_binaryOutcome_plot, fig.width = 12, fig.height = 10, out.width = "100%"----
plot(catExp.res)

## ----catExp_binaryOutcome_resultsPlot, fig.height = 3, fig.width = 5, out.width = "100%"----
ggplot(catExp.res$results.df %>% 
         filter(Parameter %in% c("Risk Difference", "Risk Ratio"))
) + 
  geom_pointrange(aes(x = Comparison, 
                      y = Estimate, 
                      ymin = `2.5% CL`, 
                      ymax = `97.5% CL`, 
                      color = Comparison), 
                  shape = 2
  ) + 
  coord_flip() + 
  facet_wrap(~Parameter, scale = "free") + 
  theme_bw() + 
  theme(legend.position = "none")

## ----catExp_binaryOutcome_predOutcomes, paged.print = FALSE-------------------
catExp.res$predicted.outcome

## ----catExp_binaryOutcome_glm.result, paged.print = FALSE---------------------

summary(catExp.res$glm.result)


## -----------------------------------------------------------------------------
sessionInfo()

