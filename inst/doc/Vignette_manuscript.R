## ---- echo = FALSE----------------------------------------
options(width = 60)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
  # tidy.opts = list(width.cutoff = 60,
  #                  args.newline = TRUE),
  # tidy = T
)

## ----installation, eval = F-------------------------------
#  install.packages("riskCommunicator")

## ----setup------------------------------------------------
library(riskCommunicator)
library(tidyverse)
library(printr)
library(formatR)
library(sandwich)
library(stringr)
library(ggpubr)

## ---- printr.help.sections = c('usage','arguments')-------
?gComp

## ---------------------------------------------------------
data(cvdd)

## ----binary_outcome, paged.print = FALSE, cache=T---------
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
                    R = 200)

binary.res

## ----gComp_class_explaination-----------------------------
class(binary.res)

## The names of the different items in the list: 
names(binary.res)

## Sample size of the original data:
binary.res$n 

## Contrast being compared in the analysis:
binary.res$contrast

## ---------------------------------------------------------
summary(binary.res)

## ----binary_outcome_plot, fig.width = 12, fig.height = 9, out.width = "100%"----
plot(binary.res)

## ----std-regression-odds-ratio, echo = F------------------
std.reg.or = glm(formula = cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP, 
                 data = cvdd, 
                 family=binomial(link='logit'))

std.reg.or

df.std.reg.or = data.frame(exp(cbind(OR = coef(std.reg.or), confint.default(std.reg.or, level = 0.95)))) %>%
  # and we pull out only the estimate we are concerned with for the predictor DIABETES
  filter(rownames(.) == "DIABETES1") %>%
  rename(LL = `X2.5..`, UL = `X97.5..`)


## ----std-regression-risk-ratio, echo = F------------------
std.reg.rr = glm(formula = cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP, 
                 ## to use Poisson regression, we need to change DIABETES from a factor to a numeric (0,1) variable
                 data = cvdd %>%
                   mutate(cvd_dth = ifelse(cvd_dth == "0", 0, ifelse(cvd_dth == "1", 1, NA))), 
                 family="poisson")
std.reg.rr

## calculate robust Standard errors
cov.std.reg.rr = sandwich::vcovHC(std.reg.rr)
std.err <- sqrt(diag(cov.std.reg.rr))

df.std.reg.rr = data.frame(Estimate = exp(coef(std.reg.rr)[2]), 
                      Robust_SE = exp(std.err[2]),
                      LL = exp(coef(std.reg.rr)[2] - 1.96 * std.err[2]), 
                      UL = exp(coef(std.reg.rr)[2] + 1.96 * std.err[2]))


## ----std-regression-risk-difference, eval = F-------------
#  std.reg.rd = glm(formula = cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP,
#                   data = cvdd %>%
#                     ## To use linear regression, we need to change DIABETES from a
#                     ##   factor to a numeric (0,1) variable
#                     mutate(cvd_dth = ifelse(cvd_dth == "0", 0,
#                                             ifelse(cvd_dth == "1", 1, NA))),
#                   family=gaussian(link = 'log'))

## ----table 2----------------------------------------------
# combine standard regression results
std.regression.res = df.std.reg.or %>%
  mutate(Parameter = "Odds Ratio",
         Std_regression = paste0(round(OR, 2), 
                                 " (", 
                                 round(LL, 2), 
                                 ", ", 
                                 round(UL, 2), 
                                 ")")) %>%
  bind_rows(df.std.reg.rr %>%
              mutate(Parameter = "Risk Ratio",
                     Std_regression = paste0(round(Estimate, 2), 
                                             " (", 
                                             round(LL, 2), 
                                             ", ", 
                                             round(UL, 2), 
                                             ")"))) %>%
  select(Parameter, Std_regression) %>%
  rename(`Standard regression` = Std_regression)
rownames(std.regression.res) = NULL


(table2 = binary.res$results.df %>%
  mutate(riskCommunicator = paste0(format(round(Estimate, 2), 2), 
                                   " (", 
                                   format(round(`2.5% CL`, 2), 2), 
                                   ", ", 
                                   format(round(`97.5% CL`, 2), 2), 
                                   ")"),
         riskCommunicator = ifelse(Parameter == "Number needed to treat/harm", 
                                   round(Estimate, 2), riskCommunicator)) %>%
  select(Parameter, riskCommunicator) %>%
  left_join(std.regression.res, by = "Parameter"))

## ----rate_outcome, cache = T------------------------------
## Modify the dataset to change the variable cvd_dth from a factor 
##  to a numeric variable since the outcome for Poisson
##  regression must be numeric.
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
                  R = 200)

rate.res

## ----rate_outcome_subgroup, cache = T---------------------

rate.res.subgroup <- gComp(data = cvdd.t, 
                           Y = "cvd_dth", 
                           X = "DIABETES", 
                           Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                           subgroup = "SEX", 
                           outcome.type = "rate", 
                           rate.multiplier = 365.25*100, 
                           offset = "timeout", 
                           R = 200)

rate.res.subgroup


## ----plot-rate-outcome-results, warning = F---------------
# Saving the output and modifying the labels of the SEX variable to specify 
#   male/female instead of 0/1
# Also adding a new variable to show the line indicating no difference found 
#   (0 for rate difference, 1 for rate ratio)

df = rate.res.subgroup$results.df %>%
  mutate(Subgroup = ifelse(Subgroup == "SEX0", "Male", "Female"),
         hline = ifelse(Parameter == "Incidence Rate Ratio", 1, 0)) 

ggplot(df, aes(x = Subgroup, y = Estimate)) + 
  geom_point(size = 2) + 
  geom_errorbar(aes(ymin = `2.5% CL`, ymax = `97.5% CL`), 
                width = 0.2) + 
  facet_wrap(~Parameter) +
  theme_bw() + 
  labs(x = "", color = "") + 
  geom_hline(aes(yintercept = hline), 
             color = "red", 
             linetype = "dashed", 
             alpha = 0.3) 

## ----std-regression-models-rate---------------------------
# Standard Poisson regression (spr) for the rate question, across both sexes
spr.rate = glm(
  formula = cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP + 
    offset(log(timeout+0.001)),
  data = cvdd.t, 
  family = "poisson"
  )

# get the parameter estimate and CI from the model object
df.spr.rate = as.data.frame(
  exp(cbind(Estimate = coef(spr.rate), confint.default(spr.rate, level = 0.95)))
  ) %>%
  rename(`2.5% CL` = `2.5 %`, `97.5% CL` = `97.5 %`) %>%
  filter(rownames(.) == "DIABETES1") %>%
  mutate(Subgroup = "All")


# Standard Poisson regression (spr) for the rate question, by subgroup (SEX)
spr.rate.subgroup = glm(
  formula = cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP + 
    DIABETES*SEX + offset(log(timeout+0.001)), 
  data = cvdd.t, 
  family = "poisson"
  )

# Get the variance-covariance matrix so we can calculate standard errors
se.subgroup = vcov(spr.rate.subgroup)

# Get estimates and CIs
df.spr.rate.subgroup = data.frame(
  Subgroup = c("Male", "Female"),
  raw.est = c(coef(spr.rate.subgroup)[2], 
              coef(spr.rate.subgroup)[2] + coef(spr.rate.subgroup)[8]),
  SE = c(sqrt(se.subgroup[2,2]), 
         sqrt(se.subgroup[2,2] + se.subgroup[8,8] + 2*se.subgroup[2,8]))) %>%
  mutate(Estimate = exp(raw.est), 
         `2.5% CL` = exp(raw.est - 1.96 * SE),
         `97.5% CL` = exp(raw.est + 1.96 * SE))

# Combine the results from the subgroup and full model into a single data.frame
combined.std.reg = df.spr.rate %>%
  bind_rows(df.spr.rate.subgroup %>%
            select(Subgroup, Estimate:`97.5% CL`)) %>%
  mutate(Parameter = "Incidence Rate Ratio",
         model = "Standard Poisson Regression")

## ----plot-rate-outcome-results-v-std-regression, warning = F, fig.width = 8, fig.height = 4----
# Combine the riskCommunicator results with the standard Poisson regression results
df.combined = rate.res$results.df %>%
  mutate(Subgroup = "All") %>%
  bind_rows(df) %>%
  mutate(model = "riskCommunicator") %>%
  select(-Outcome, -Comparison) %>%
  bind_rows(combined.std.reg) %>%
  mutate(hline = ifelse(Parameter == "Incidence Rate Ratio", 1, 0))


rate.diff = ggplot(df.combined %>% 
                      filter(Parameter == "Incidence Rate Difference"), 
                   aes(x = Subgroup, y = Estimate, color = model)) + 
  geom_point(size = 2, position = position_dodge(width = .5)) + 
  geom_errorbar(aes(ymin = `2.5% CL`, ymax = `97.5% CL`), 
                width = 0.2, 
                position = position_dodge(width = .5)) + 
  scale_color_manual(values = c("#481567FF", "#3CBB75FF")) + 
  theme_bw() + 
  labs(x = "",  
       y = str_wrap('Incidence rate difference of 
                    cardiovascular disease or death 
                    (cases/100 person-years)', width = 32),
       color = "") + 
  geom_hline(aes(yintercept = hline), 
             color = "red", 
             linetype = "dashed", 
             alpha = 0.3) + 
  theme(legend.position = "none") 

rate.ratio = ggplot(df.combined %>% 
                       filter(Parameter == "Incidence Rate Ratio"), 
                   aes(x = Subgroup, y = Estimate, color = model)) + 
  geom_point(size = 2, position = position_dodge(width = .5)) + 
  geom_errorbar(aes(ymin = `2.5% CL`, ymax = `97.5% CL`), 
                width = 0.2, 
                position = position_dodge(width = .5)) + 
  scale_y_continuous(trans = "log2") +
  scale_color_manual(values = c("#481567FF", "#3CBB75FF")) + 
  theme_bw() + 
  labs(x = "",  
       y = str_wrap('Incidence rate ratio of 
                    cardiovascular disease or death 
                    (shown on natural log scale)', width = 32),
       color = "") + 
  geom_hline(aes(yintercept = hline), 
             color = "red", 
             linetype = "dashed", 
             alpha = 0.3) + 
  theme(legend.position = "bottom") 

ggarrange(rate.ratio, rate.diff, ncol = 2, common.legend = T, legend = "bottom",
          labels = c("A", "B"), widths = c(1, 1))

## ----save-Fig1, include = F, eval = F---------------------
#  ggsave(filename = "~/Dropbox/Framingham teaching data set/Manuscript drafts/Fig1.tiff", device = "tiff", width = 10, height = 6)

## ---------------------------------------------------------
sessionInfo()

