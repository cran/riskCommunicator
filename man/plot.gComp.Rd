% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.gComp.R
\name{plot.gComp}
\alias{plot.gComp}
\title{Plot estimates of difference and ratio effects obtained in the bootstrap
computations of the g-computation}
\usage{
\method{plot}{gComp}(x, ...)
}
\arguments{
\item{x}{(Required) An object of class \code{gComp}.}

\item{...}{(Optional) additional arguments to be supplied to the
`\code{geom_histogram} call (e.g. to adjust binwidth for histogram, assign
colors, etc.).}
}
\value{
a plot containing histograms and Q-Q plots of the difference and
  ratio estimates returned from R bootstrap iterations
}
\description{
Plot histograms and Q-Q plots for each the difference and ratio
  estimates
}
\examples{
## Obtain the risk difference and risk ratio for cardiovascular disease or death
## between patients with and without diabetes, while controlling for
## age,
## sex,
## BMI,
## whether the individual is currently a smoker, and
## if they have a history of hypertension.
data(cvdd)
set.seed(58)
diabetes.result <- gComp(data = cvdd, Y = "cvd_dth", X = "DIABETES",
Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), outcome.type = "binary", R = 60)
plot(diabetes.result)

}
\keyword{plot.gComp}
