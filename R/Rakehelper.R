#' Rakehelper: Quick and Flexible Rake Weighting
#'
#' @description Rakehelper is a package for quickly and flexibly calculating
#'   rake weights (also know as rim weights). It is
#'   designed to interact with \code{\link[survey]{svydesign}} objects and other
#'   functionalities from Thomas Lumley's "survey" package.
#'
#' @section Rake weighting concepts:
#'   Post-stratification weights are commonly used in survey research to ensure
#'   that sample is representative of the population it is drawn from, in cases
#'   where some people selected for inclusion in a sample might decline to
#'   participate. To calculate post-stratification weights, observed categorical
#'   variables in a survey dataset (usually demographic variables) must be
#'   matched with "targets" (typically known population demographics from census
#'   data). Survey respondents from underrepresented categories are upweighted,
#'   while respondents from overrepresented categories are downweighted.
#'
#'   Rakehelper focuses particularly on "rake" or "rim" weighting (sometimes
#'   known more formally as iterative proportional fitting). This is a
#'   widely-used method for simultaneously calculating weights on multiple
#'   variables, when no join distribution for these variables is known. For
#'   example, population data on past vote (from election results) and age (from
#'   the census) are generally known. However, as the joint distribution of past
#'   vote and age is \emph{not} generally known, a technique such as rake
#'   weighting must be used to apply weights on both variables simultaneously.
#'
#' @section Package features:
#'  The core function in Rakehelper is \code{\link{rakesvy}} (and the related \code{\link{rakew8}}), 
#'   which calculates post-stratification weights for a dataset or \code{svydesign} object,
#'   given targets. The command is designed to make weighting as simple as
#'   possible, with the following features:
#'  \itemize{ 
#'    \item Weighting to either counts or percentage targets 
#'    \item Allowing specification of targets as vectors, matrices, or data frames
#'    \item Accepting targets of 0 (equivalent to dropping cases from analysis) 
#'    \item Allowing targets to be quickly rebased a specified sample size 
#'    \item Flexibly matching targets to the correct variables in a dataset
#'   }
#'
#'  The package does this in part by introducing the \code{\link{w8margin}}
#'  object class. A w8margin is a desired raw \emph{count} of categories for a
#'  variable, in the format required for actually computing weights. 
#'  However, this format is somewhat cumbersome to specify manually. The package includes methods 
#'  for converting named vectors, matrices, and data frames to w8margin object;
#'   \code{rakesvy} and \code{rakew8} call these methods automatically.
#'
#'   At present, the core weighting calculations are actually performed via the
#'   survey package's \code{\link[survey]{rake}} function. This is expected
#'   to change with future releases, although the basic approach to iterative 
#'   weighting is not expected to change.
#'   
#'   The package is under active development, and additional features are
#'   planned for future release. Major plans include adding techniques for more
#'   robustly weighting ordinal and numeric data; allowing targets with
#'   \code{NA} values on some categories, and allowing targets to specify
#'   recodes of observed variables in \code{rakesvy}. Contributions to the package,
#'   or suggestions for additional features, are gratefully accepted via email
#'   or github.
#'   
#' @author Ben Mainwaring (\email{mainwaringb@@gmail.com}, \url{https://www.linkedin.com/in/mainwaringb})
#'   
#' @seealso Package GitHub repository: \url{https://github.com/mainwaringb/rakehelper}
#'   
#' @docType package
#' @name Rakehelper
NULL