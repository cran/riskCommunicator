#' riskCommunicator: Obtaining interpretable epidemiological effect
#' estimates
#'
#' \code{riskCommunicator} is a package for estimating flexible epidemiological
#' effect measures including both differences and ratios. The package is based
#' on the parametric G-formula (g-computation with parametric models) developed
#' by Robbins et. al. in 1986 as an alternative to inverse probability weighting.  
#' It is useful for estimating the impact of interventions in the presence of 
#' treatment-confounder-feedback and is a powerful tool for causal inference, 
#' but has seen limited success due to lack of software for the computationally 
#' intensive components. This package provides three main functions.  
#' The first, \code{pointEstimate}, obtains a point estimate of the difference 
#' and ratio effect estimates. This function is typically called within the 
#' \code{gComp} function, but is available for use in special cases for example 
#' when the user requires more explicit control over bootstrap resampling 
#' (e.g. nested clusters). The second function, \code{gComp}, is the workhorse 
#' function that obtains point estimates for difference and ratio effects along 
#' with their 95/% confidence intervals. The \code{plot} function allows users 
#' to visualize the bootstrap results. We provide the \code{framingham} dataset, 
#' which is the teaching dataset from the Framingham Heart Study, as well as a 
#' subset of that data, \code{cvdd} for users.
#' @references Robins, James. 1986. “A New Approach To Causal Inference in
#' Mortality Studies with a Sustained Exposure Period - Application To Control
#' of the Healthy Worker Survivor Effect.” Mathematical Modelling 7: 1393–1512.
#' doi:10.1016/0270-0255(86)90088-6.
#'
#' @seealso \code{\link{gComp}}
#' @seealso \code{\link{pointEstimate}}
#' @seealso \code{\link{plot.gComp}}
#' @docType package
#' @name riskCommunicator
#'   
NULL
utils::globalVariables(names = c("value",
                                 ".",
                                 ":="))