% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_results_dataframe.R
\name{get_results_dataframe}
\alias{get_results_dataframe}
\title{Take predicted dataframe and calculate the outcome (risk difference/ratio,
incidence rate difference/ratio, mean difference, and/or number needed to
treat)}
\usage{
get_results_dataframe(predict.df, outcome.type, rate.multiplier)
}
\arguments{
\item{predict.df}{(Required) A data.frame output from the
\code{make_predict_df} function with predicted outcome for each observation
at each level of treatment/exposure.}

\item{outcome.type}{(Required) Character argument to describe the outcome
type. Acceptable responses, and the corresponding error distribution and
link function used in the \code{glm}, include: \describe{
\item{binary}{(Default) A binomial distribution with link = 'logit' is
used.} \item{count}{A Poisson distribution with link = 'log' is used.}
\item{rate}{A Poisson distribution with link = 'log' is used.}
\item{continuous}{A gaussian distribution with link = 'identity' is used.}
}}

\item{rate.multiplier}{(Optional, only applicable for rate outcomes) Default
1. Numeric value to multiply to the rate-based effect measures. This option
facilitates reporting effects with interpretable person-time denominators.
For example, if the person-time variable (offset) is in days, a multiplier
of 365*100 would result in estimates of rate differences per 100
person-years.}
}
\value{
A list containing the calculated results for the applicable measures
  (based on the outcome.type): Risk Difference, Risk Ratio, Odds Ratio,
  Incidence Risk Difference, Incidence Risk Ratio, Mean Difference, Number
  Needed to Treat, Average Tx (average predicted outcome of all observations with 
  treatment/exposure), and Average noTx (average predicted outcome of all 
  observations without treatment/exposure)
}
\description{
Take predicted dataframe and calculate the outcome (risk difference/ratio,
incidence rate difference/ratio, mean difference, and/or number needed to
treat)
}