% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_functions.R
\name{eff_n}
\alias{eff_n}
\alias{weight_eff}
\title{Effective Sample Size and Weighting Efficiency}
\usage{
eff_n(design)

weight_eff(design)
}
\arguments{
\item{design}{An \code{\link[survey]{svydesign}} object, presumably with
design or post-stratification weights.}
}
\value{
A numeric value, indicating effective sample size (for \code{eff_n()})
or weighting efficiency (for \code{weight_eff()})
}
\description{
Computes Kish's effective sample size or weighting efficiency for a
\code{survey.design} object.
}
\details{
Kish's effective sample size is a frequently-used, general metric to
indicate how much uncertainty and error increase due to weighting.
Effective sample size is calculated as \code{sum(weights(design))^2 / sum(weights(design)^2)}.
Weighting efficiency is \code{eff_n(design) / sum(weights(design))}.

While weighting efficency and effective sample size are frequently use,
they are less valid than the standard errors produced by
\code{\link[survey:surveysummary]{survey::svymean()}} and related functions from the {survey}
package. In particular, they ignore clustering and stratification in
sample designs, and covariance between weighting variables and outcome variables.
As such, these metrics should be used with caution
}
\examples{
gles17_weighted <- rakesvy(design = gles17, 
    gender ~ c("Male" = .495, "Female" = .505),
    eastwest ~ c("East Germany" = .195, "West Germany" = .805)
)

eff_n(gles17_weighted)
weight_eff(gles17_weighted)
}
\references{
Kish, Leslie. 1965. \emph{Survey Sampling} New York: Wiley.
}
