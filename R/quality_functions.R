## ==== KISH'S EFFECTIVE SAMPLE SIZE ====

#' Effective Sample Size and Weighting Efficiency
#' @description Computes Kish's effective sample size or weighting efficiency for a
#'   \code{\link[survey]{svydesign}} object. 
#' @param x Either an object from \code{\link[survey]{svydesign}} (with
#'   weights), or a vector of case weights.
#' @details Kish's effective sample size is a frequently-used, general metric to
#'   indicate how much uncertainty and error increase due to weighting. 
#'   Effective sample size is calculated as \code{sum(weights(design))^2 / sum(weights(design)^2)}. 
#'   Weighting efficiency is \code{eff_n(design) / sum(weights(design))}.
#' @details While weighting efficiency and effective sample size are frequently used,
#'  they are less valid than the standard errors produced by
#'   \code{\link[survey]{svymean}} and related functions from the {survey}
#'   package. In particular, they ignore clustering and stratification in 
#'   sample designs, and covariance between weighting variables and outcome variables.
#'   As such, these metrics should be used with caution
#' @example inst/examples/eff_n_examples.R
#' @export
eff_n <- function(x){
    UseMethod("eff_n")
}

#' @rdname eff_n
#' @export
eff_n.survey.design <- function(x){
    myweights <- weights.survey.design(x)
    eff_n <- eff_n.numeric(myweights)
    return(eff_n)
}

#' @rdname eff_n
#' @export
eff_n.numeric <- function(x){
    eff_n <- (sum(x) ^ 2) / (sum(x ^ 2))
    return(eff_n)
}

#' @rdname eff_n
#' @export
weight_eff <- function(x){
    UseMethod("weight_eff")
}

#' @rdname eff_n
#' @export
weight_eff.survey.design <- function(x){
    out <- eff_n(x) / sum(weights.survey.design(x))
    return(out)
}

#' @rdname eff_n
#' @export
weight_eff.numeric <- function(x){
    out <- eff_n(x) / sum(x)
    return(out)
}
