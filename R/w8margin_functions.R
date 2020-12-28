#major to-do list:
#nice to have
#as.w8margin
    #1) think about as.w8margin.w8margin and as.w8margin.array
    #2) create shared "checkTolerance" function
    #3) move core of as.w8margin.matrix to calling as.w8margin.vector
#w8margin.matched:
    #1) accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables


## ==== FUNCTIONS TO CONVERT MATRICES, DFS, AND VECTORS TO w8margins ====

#TO DO: 
#create shared checkTolerance function, called by all methods
#think about as.w8margin.w8margin and as.w8margin.array

#' Weight Margin Objects
#'
#' @description Creates an object of class \code{w8margin}, representing the
#'   desired target distribution of a single categorical variable after
#'   weighting. Used as input for \code{\link{rakesvy}}, \code{\link{rakew8}},
#'   \code{\link[survey]{rake}}, and \code{\link[survey]{postStratify}}
#'   functions. Methods exist for numeric vectors, matrices, and data frames
#'   (see details).
#' @usage as.w8margin(target, varname, levels = NULL, samplesize = NULL,
#'   rebase.tol = .01)
#' @param target Numbers specifying the desired target distribution of a
#'   categorical variable, after rake weighting. Can be a numeric vector,
#'   numeric matrix, or data frame with one (and only one) numeric column.
#'   Unless \code{levels} is specified, vectors and matrices must be named, and
#'   data frames must have a character or factor column specifying names. See
#'   details.
#' @param varname Character vector specifying the name of the observed variable
#'   that the \code{w8margin} object should match.
#' @param levels Optional character vector, specifying which numeric elements of
#'   \code{target} match with each factor level in the observed data. Overrides
#'   names specified in \code{target}.
#' @param samplesize  Numeric with the desired target sample size for the
#'   w8margin object. Defaults to the sum of \code{target}.
#' @param rebase.tol Numeric between 0 and 1. If targets are rebased, and the
#'   rebased sample sizes differs from the original sample size by more than
#'   this percentage, generates a warning.
#' @param byrow Logical, specifying how matrix targets should be converted to vectors. 
#'   If FALSE, the elements within columns will be adjacent
#'   in the resulting w8margin object, otherwise elements within rows will be
#'   adjacent.
#' @details w8margin objects are inputs to the \code{\link{rakesvy}},
#'   \code{\link{rakew8}}, \code{\link[survey]{rake}}, and
#'   \code{\link[survey]{postStratify}} functions. These functions require a
#'   specific, highly-structured input format. For flexibility,
#'   \code{as.w8margin} can be used to convert a variety of common inputs into
#'   the format needed by these functions.
#' @details \code{as.w8margin} has methods for numeric vectors, numeric matrices, and
#'   data frames. Each method has multiple ways of determining how to match
#'   numeric elements of \code{target} with factor levels in the observed data.
#'   For numeric vector and matrix inputs, the default is to match based on the
#'   name of each element (for vectors) or the interaction of row and column
#'   names of each element (for matrices). These names can be overridden by
#'   specifying the \code{levels} parameter.
#' @details Data frame inputs must have either one or two columns. Two-column
#'   data frames must have one numeric column and one character column. The
#'   numeric column specifies the target distribution, while the character
#'   column specifies how to match numeric elements with factor levels in the
#'   observed data. If \code{varname} is NULL, a default value will be taken
#'   from the name of the non-numeric column.
#' @details One-column data frames must have a numeric column. Row names are
#'   converted to a character column in order to match numeric elements with
#'   factor levels in the observed data. One-column data frames must specify a
#'   \code{varname} parameter, and (unless \code{levels} is specified) must have
#'   non-default row names. The \code{levels} parameter can be used with both
#' one- and two-column data frames.
#' @details Technically, \code{w8target} objects are data frames with two
#'   columns. The first column specifies levels in the observed factor variable,
#'   and the *name* of the first column indicates the name of the observed
#'   factor variable. The second column is named "Freq" and indicates the
#'   desired post-raking frequency of each category (as a *count* rather than percentage). 
#'   The structure is designed for compatibility with the survey package.
#'   Because frequency is specified as a count, \code{\link{rakesvy}} and \code{\link{rakew8}} 
#'   re-call \code{as.w8margin} whenever weighting a data set to a new observed sample size. 
#'   Weight margins must be manually re-calculated for new sample sizes when using
#'   \code{\link[survey]{postStratify}} or \code{\link[survey]{rake}}.
#' @return An object of class w8margin, with specified variable name and sample size.
#' @aliases w8margin
#' @export
as.w8margin <- function(x, ...){
    UseMethod("as.w8margin")
}

#' @rdname as.w8margin
#' @export
as.w8margin.matrix <- function(target, varname, levels = NULL, samplesize = NULL, byrow = TRUE, rebase.tol = .01){
  target.matrix <- target
  forcedLevels <- levels #Internally, we will use a forcedLevels parameter to avoid confusion with the levels function
  
  ## ---- set names for target levels ----
  target.vector <- gdata::unmatrix(target.matrix, byrow = byrow)
  if(is.null(forcedLevels)){  #set w8margin levels based on row and column names, if "forcedLevels" is not specified
    if(sum(is.na(rownames(target.matrix))) > 0 | is.null(rownames(target.matrix))) stop("Matrix has invalid or missing row names")
    if(sum(is.na(colnames(target.matrix))) > 0 | is.null(colnames(target.matrix))) stop("Matrix has invalid or missing column names")
    names(target.vector) <- gsub(":", ".", names(target.vector))
  } else{  #set w8margin levels based on forcedLevels input, if it us specified
    if(length(forcedLevels) != length(target.vector)) stop("levels must be of length ", length(target.vector))
    names(target.vector) <- forcedLevels
  }
  
  duplicates <- duplicated(names(target.vector))
  if(sum(duplicates) > 0) stop("Duplicated target level(s) ", toString(names(target.vector[duplicates]), sep = ", "))
  
  NAs <- is.na(target.vector)
  if(any(NAs)) stop("Target is NA for levels(s) ", toString(names(target.vector[NAs]), sep = ", "))
  
  ## ---- rebase targets to sample size ----
  origSum <- sum(target.vector)
  if(is.null(samplesize)){
    samplesize <- origSum
  }else{ #generate a warning message if the original target doesn't sum to 1, 100, or samplesize (+- some tolerance)
    checkTolerance.vec <- c(1, 100, samplesize) / origSum #Compute the ratio of 1, 100, and the original sample size to OrigSum
    isTolerated <- sum(checkTolerance.vec > (1 - rebase.tol) & checkTolerance.vec < (1 + rebase.tol)) #check if the ratio is 1 +- some tolerancee
    if(isTolerated == FALSE) warning("targets for variable ", varname, " sum to ", origSum, " and were rebased")
  }
  target.counts <- (target.vector / origSum) * samplesize 
  
  ## ---- generate output object ----
  w8margin <- data.frame(names(target.counts), target.counts)
  names(w8margin) <- c(varname, "Freq")
  
  class(w8margin) <- c("w8margin", "data.frame")
  return(w8margin)
}

#' @rdname as.w8margin
#' @export
as.w8margin.data.frame <- function(target, varname = NULL, levels = NULL, samplesize = NULL, rebase.tol = .01){
  target.df <- target
  forcedLevels <- levels
  
  ## ---- error handling ----
  if(ncol(target.df) > 2 | ncol(target.df) == 0) stop("Data frames must have one or two columns for conversion to w8margin")

  if(ncol(target.df) == 1){#If data frame has one column (Freq) and row names, convert row names into column
      if(all(rownames(target.df) == 1:nrow(target.df)) & is.null(forcedLevels)) stop("One-column data frames must have non-default row names for conversion to w8margin, unless levels are specified")
      if(is.null(varname)) stop("One-column data frames must have specified varname")
      if(!("numeric" %in% class(target.df[,1]))) stop("One-column data frame must have numeric variable for conversion to w8margin")
      
      warning("Coercing row names ", toString(rownames(target.df)), " to variable level names")
      
      colnames(target.df) <- "Freq"
      target.df <- cbind(rownames(target.df), target.df)
      names(target.df)[1] <- varname
  } else if(ncol(target.df) == 2){
      isNumeric <- sapply(target.df, is.numeric)
      if(sum(isNumeric) != 1) stop("Two-column data frames must have exactly one numeric column for conversion to w8margin")
      names(target.df)[isNumeric] <- "Freq"
      
      if(!is.null(varname)) names(target.df)[!isNumeric] <- varname
      if(is.null(varname)) varname <- names(target.df)[!isNumeric]
      
  }
  
  if(!(is.null(forcedLevels))){
      if(length(forcedLevels) != nrow(target.df)) stop("levels must be of length ", nrow(target.df))
      target.df[names(target.df) != "Freq"] <- forcedLevels
  }
  
  target_levels <- target.df[,names(target.df) != "Freq"]
  duplicates <- duplicated(target_levels)
  if(sum(duplicates) > 0) stop("Duplicated target level(s) ", toString(target_levels[duplicates], sep = ", "))

  NAs <- is.na(target.df[,2])
  if(any(NAs)) stop("Target is NA for level(s) ", toString(target_levels[NAs]), sep = ", ")
  
  ## ---- rebase targets to sample size ----
  w8margin <- target.df
  origSum <- sum(target.df$Freq)
  if(is.null(samplesize)){
    samplesize <- origSum
  }else{ #generate a warning message if the original target doesn't sum to 1, 100, or samplesize (+- some tolerance)
    checkTolerance.vec <- c(1, 100, samplesize) / origSum #Compute the ratio of 1, 100, and the original sample size to OrigSum
    isTolerated <- sum(checkTolerance.vec > (1 - rebase.tol) & checkTolerance.vec < (1 + rebase.tol)) #check if the ratio is 1 +- some tolerancee
    if(isTolerated == FALSE) warning("targets for variable ", varname, " sum to ", origSum, " and were rebased")
  }
  w8margin$Freq <- (target.df$Freq / origSum) * samplesize #rebase targets to sample size
  
  ## ---- generate output object ----
  
  class(w8margin) <- c("w8margin", "data.frame")
  return(w8margin)
}

#' @rdname as.w8margin
#' @export
as.w8margin.numeric <- function(target, varname, levels = NULL, samplesize = NULL, rebase.tol = .01){
  target.numeric <- target
  forcedLevels <- levels
  
  ## ---- error handling ----
  if(is.null(forcedLevels)){
    if(sum(is.na(names(target.numeric))) > 0 | is.null(names(target.numeric))) stop("Vector has invalid or missing names; try specifying levels")
  } else{
    if(length(forcedLevels) != length(target.numeric)) stop("levels must be of length ", length(target.numeric))
    names(target.numeric) <- forcedLevels
  }
  duplicates <- duplicated(names(target.numeric))
  if(sum(duplicates) > 0) stop("Duplicate target level(s) ", toString(names(target.numeric[duplicates]), sep = ", "))
  
  NAs <- is.na(target.numeric)
  if(any(NAs)) stop("Target is NA for level(s) ", toString(names(target.numeric[NAs])), sep = ", ")
  
  ## ---- rebase targets to sample size ----
  origSum <- sum(target.numeric)
  if(is.null(samplesize)){
    samplesize <- origSum
  }else{ #generate a warning message if the original target doesn't sum to 1, 100, or samplesize (+- some tolerance)
    checkTolerance.vec <- c(1, 100, samplesize) / origSum #Compute the ratio of 1, 100, and the original sample size to OrigSum
    isTolerated <- sum(checkTolerance.vec > (1 - rebase.tol) & checkTolerance.vec < (1 + rebase.tol)) #check if the ratio is 1 +- some tolerancee
    if(isTolerated == FALSE) warning("Targets for variable ", varname, " sum to ", origSum, " and were rebased")
  }
  target.counts <- (target.numeric / origSum) * samplesize #rebase targets to sample size
  
  ## ---- generate output object ----
  w8margin <- data.frame(names(target.counts), target.counts)
  names(w8margin) <- c(varname, "Freq")
  
  class(w8margin) <- c("w8margin", "data.frame")
  return(w8margin)
}


## ==== w8margin.matched ====

#TO DO:
#accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables

#' Check Whether w8margin Object Matches Observed Variable
#' @description Checks whether specified \code{\link{w8margin}} object and variable in observed
#'   data are compatible, and are expected to produce valid call to
#'   \code{\link[survey]{rake}}. Returns a logical true/false, and generates
#'   warning messages to specify likely issues. Intended to help quickly
#'   diagnose incompatibilities between w8margins and observed data.
#' @usage w8margin.matched(w8margin, observed, refactor = FALSE)
#' @param w8margin w8margin object, or other object type that can be coerced to
#'   w8margin with a temporary variable name.
#' @param observed factor variable (or, if \code{refactor = TRUE}, a variable that can
#'   be coerced to factor).
#' @param refactor logical, specifying whether to factor observed variable before checking
#'   match.
#' @return A logical, indicating whether w8margin is compatible with observed.
#' @export
w8margin.matched <- function(w8margin, observed, refactor = FALSE){
  
  ## --- Error handling ----
  if(is.factor(observed) == FALSE){
    if(refactor == FALSE){
        warning("observed data is not a factor variable")
        return(FALSE)
    }
  }
  if(refactor == TRUE) observed <- factor(observed)
  obs_levels <- levels(observed)
  
  if(!("w8margin" %in% class(w8margin))){
    w8margin <- as.w8margin(w8margin, varname = "(unnamed target)")
  }
  targetname <- colnames(w8margin)[1]
  
  ## ---- Check for NAs in observed data and target ----
  if(any(is.na(observed))){
      warning("NAs in observed data for ", targetname)
      return(FALSE)
  }
  if(any(is.na(w8margin[,2]))){
      warning("Target is NA for levels() ", toString(w8margin[is.na(w8margin[,2]),1], sep = ", "), " on variable ", targetname)
      return(FALSE)
  }
  
  ## ---- Check for empty levels in observed and target ----
  emptyObserved <- table(observed) == 0
  hasEmptyObserved <- sum(emptyObserved)
  emptyTarget <- w8margin$Freq == 0
  hasEmptyTarget <- sum(emptyTarget)
  
  if(hasEmptyObserved > 0 | hasEmptyTarget > 0){
      if(hasEmptyObserved > 0) warning("Observed data for ", targetname, " contains empty factor level ", paste(levels(observed)[emptyObserved], collapse = ", "))
      if(hasEmptyTarget > 0)  warning("Weight target ", targetname, " contains empty factor level ", paste(w8margin[emptyTarget,1], collapse = ", "))
      return(FALSE)
  }
  
  
  ## ---- Check if number of levels in observed data matches length of target ----
  if(length(w8margin[,1]) != length(obs_levels)){
    warning("Number of variable levels in observed data does not match length of target ", targetname)
    return(FALSE)
  }
  
  ## ---- Check for levels in observed data that do not match levels in target ----
  
  #otherwise, check if *sorted* variable levels are the same
  if(sum(sort(as.character(w8margin[,1])) != sort(obs_levels)) > 0){
    #Identify missing levels in both observed and target 
    missing_from_target.index <- !(w8margin[,1] %in% obs_levels)
    missing_from_obs.index <- !(obs_levels %in% w8margin[,1])
    missing_from_target.string <- paste(w8margin[missing_from_target.index, 1], collapse = ", ")
    missing_from_obs.string <- paste(obs_levels[missing_from_obs.index], collapse = ", ")
    
    if(sum(missing_from_target.index) > 0) warning("variable levels ", toString(missing_from_target.string, sep = ", "), " in target ", targetname, " are missing from observed factor variable")
    if(sum(missing_from_obs.index) > 0) warning("variable levels ", toString(missing_from_obs.string, sep = ", "), " in observed factor variable are missing from target ", targetname)
    
    return(FALSE)
  }
    
  #If all checks pass, return TRUE
  return(TRUE)
}


## ==== MISCELLANEOUS FUNCTIONS ====

#' Kish's Effective Sample Size
#' @description Computes Kish's effective sample size for a
#'   \code{\link[survey]{svydesign}} object. Computed using the formula
#'   \code{sum(weights(design)) ^ 2 / sum(weights(design) ^ 2)}.
#' @usage eff.n(design)
#' @param design An \code{\link[survey]{svydesign}} object, presumably with
#'   design or post-stratification weights.
#' @details Kish's effective sample size is a frequently-used, general metric to
#'   indicate how much uncertainty and error increase due to weighting. However,
#'   it is less valid than the standard errors produced by
#'   \code{\link[survey]{svymean}} and related functions from the {survey}
#'   package: it ignores clustering and stratification in sample designs, and
#'   covariance between weighting variables and outcome variables.
#' @details The *weighting efficiency* of a sample is a closely related metric,
#'   which shares all the problems of effective sample size. It can be
#'   calculated as \code{eff.n(design) / sum(weights(design))}.
#' @export
eff.n <- function(design){
  myweights <- survey:::weights.survey.design(design)
  eff.n <- (sum(myweights) ^ 2) / (sum(myweights ^ 2))
  return(eff.n)
}


## ==== INTERNAL FUNCTIONS ====

# ---- check target length ----
target.length <- function(x, ...){
  UseMethod("target.length")
}

target.length.numeric <- function(w8margin){length(w8margin)}
target.length.data.frame <- function(w8margin){nrow(w8margin)}
target.length.matrix <- function(w8margin){nrow(w8margin) * ncol(w8margin)}



