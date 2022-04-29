## ==== KISH'S EFFECTIVE SAMPLE SIZE ====

#' Effective Sample Size and Weighting Efficiency
#' @description Computes Kish's effective sample size or weighting efficiency for a
#'   \code{survey.design} object. 
#' @param design An \code{\link[survey]{svydesign}} object, presumably with
#'   design or post-stratification weights.
#' @return A numeric value, indicating effective sample size (for \code{eff_n()})
#'   or weighting efficiency (for \code{weight_eff()})
#' @details Kish's effective sample size is a frequently-used, general metric to
#'   indicate how much uncertainty and error increase due to weighting. 
#'   Effective sample size is calculated as \code{sum(weights(design))^2 / sum(weights(design)^2)}. 
#'   Weighting efficiency is \code{eff_n(design) / sum(weights(design))}.
#' @details While weighting efficency and effective sample size are frequently use,
#'  they are less valid than the standard errors produced by
#'   [survey::svymean()] and related functions from the {survey}
#'   package. In particular, they ignore clustering and stratification in 
#'   sample designs, and covariance between weighting variables and outcome variables.
#'   As such, these metrics should be used with caution
#' @example inst/examples/eff_n_examples.R
#' @references Kish, Leslie. 1965. *Survey Sampling* New York: Wiley.
#' @export
eff_n <- function(design){
    myweights <- weights.survey.design(design)
    eff_n <- (sum(myweights) ^ 2) / (sum(myweights ^ 2))
    return(eff_n)
}

#' @rdname eff_n
#' @export
weight_eff <- function(design){
    out <- eff_n(design) / sum(weights.survey.design(design))
    return(out)
}
