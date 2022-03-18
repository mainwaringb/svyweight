#major to-do list:
#nice to have
#as.w8margin
    #1) think about as.w8margin.w8margin and as.w8margin.array and as.w8margin.table
    #2) check for negative numbers in as.w8margin
#w8margin_matched:
    #1) accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables

## ==== FUNCTIONS TO CONVERT MATRICES, DFS, AND VECTORS TO w8margins ====

#TO DO: 
#think about as.w8margin.w8margin and as.w8margin.array

#' Weight Margin Objects
#' 
#' @description Creates an object of class \code{w8margin}. Represents the
#'   desired target distribution of a categorical variable, after
#'   weighting (as a *counts*, not percentage). w8margin objects are in the format 
#'   required by \code{\link[survey]{rake}}, and  \code{\link[survey]{postStratify}},
#'   and are intended mostly for use with these functions. Methods exist for 
#'   numeric vectors, matrices, and data frames (see details).
#' @param target Numbers specifying the desired target distribution of a
#'   categorical variable, after rake weighting. Can be a numeric vector,
#'   numeric matrix, or data frame with one (and only one) numeric column.
#'   Unless \code{levels} is specified, vectors and matrices must be named, and
#'   data frames must have a character or factor column specifying names. See
#'   details.
#' @param varname Character vector specifying the name of the observed variable
#'   that the \code{w8margin} object should match. Can take a NULL value for 
#'   data frames, in which case the original column name is used.
#' @param levels Optional character vector, specifying which numeric elements of
#'   \code{target} match with each factor level in the observed data. Overrides
#'   names specified in \code{target}.
#' @param samplesize  Numeric with the desired target sample size for the
#'   w8margin object. Defaults to the sum of \code{target}.
#' @param na.allow Logical specifying whether NA values should be allowed in 
#'   w8margin objects. If TRUE, w8margin objects must be imputed (such as with
#'   \code{impute_w8margin}) before they can be used for weighting.
#' @param rebase.tol Numeric between 0 and 1. If targets are rebased, and the
#'   rebased sample sizes differs from the original sample size by more than
#'   this percentage, generates a warning.
#' @param byrow Logical, specifying how matrix targets should be converted to vectors. 
#'   If FALSE, the elements within columns will be adjacent
#'   in the resulting w8margin object, otherwise elements within rows will be
#'   adjacent.
#' @param ... Other method-specific arguments, currently not used
#' @details w8margin objects are inputs to the \code{\link[survey]{rake}} and
#'   \code{\link[survey]{postStratify}}. These functions require a
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
#' @example inst/examples/w8margin_examples.R
#' @aliases w8margin
#' @export
as.w8margin <- function(target, varname, levels = NULL, samplesize = NULL, na.allow = FALSE, rebase.tol = .01, ...){
    # ---- Check for valid inputs ----
    if(!is.null(samplesize)){ # If sample size is specified
        if(!("numeric" %in% class(samplesize) & samplesize > 0)){
            stop('Invalid value for samplesize; must be numeric greater than 0 or NULL')
        }
    }
        
    if(!("numeric" %in% class(rebase.tol)) | rebase.tol > 1 | rebase.tol < 0){
        stop('Invalid value for rebase.tol; must be numeric between 0 and 1')
    }
    
    if(is.na(as.logical(na.allow))){
        stop('Invalid value for na.allow; must be coercable to TRUE or FALSE')
    } else{
        na.allow <- as.logical(na.allow)
    }
        
    # ---- Call method ----
    UseMethod("as.w8margin")
}

#' @rdname as.w8margin
#' @export
as.w8margin.data.frame <- function(target, varname, levels = NULL, samplesize = NULL, na.allow = FALSE, rebase.tol = .01, ...){
  target.df <- target
  forcedLevels <- levels
  
  ## ---- error handling ----
  if(ncol(target.df) > 2 | ncol(target.df) == 0) stop("Data frames must have one or two columns for conversion to w8margin")
  if(nrow(target.df) < 1) stop("Data frames must have at least one row for conversion to w8margin")
  
  if(ncol(target.df) == 1){#If data frame has one column (Freq) and row names, convert row names into column
      if(all(rownames(target.df) == 1:nrow(target.df)) & is.null(forcedLevels)) stop("One-column data frames must have non-default row names for conversion to w8margin, unless levels are specified")
      if(!("numeric" %in% class(target.df[,1]))) stop("One-column data frame must have numeric variable for conversion to w8margin")
      
      warning("Coercing row names ", toString(rownames(target.df)), " to variable level names")
      
      if(is.null(varname)) varname <- colnames(target.df)
      colnames(target.df) <- "Freq"
      
      target.df <- cbind(rownames(target.df), target.df)
      names(target.df)[1] <- varname
  } else if(ncol(target.df) == 2){
      isNumeric <- sapply(target.df, is.numeric)
      if(sum(isNumeric) != 1) stop("Two-column data frames must have exactly one numeric column for conversion to w8margin")
      
      if(!is.null(varname)) names(target.df)[!isNumeric] <- varname
      if(is.null(varname)) varname <- names(target.df)[!isNumeric]
      
      target.df <- data.frame(target.df[,!isNumeric], target.df[,isNumeric])
      colnames(target.df) <- c(varname, "Freq")
  }
  
  if(!(is.null(forcedLevels))){
      if(length(forcedLevels) != nrow(target.df)) stop("levels must be of length ", nrow(target.df))
      target.df[names(target.df) != "Freq"] <- forcedLevels
  }
  
  # ---- Get target levels ----
  target_levels <- target.df[,names(target.df) != "Freq"]
  duplicates <- duplicated(target_levels)
  if(sum(duplicates) > 0) stop("Duplicated target level(s) ", toString(target_levels[duplicates], sep = ", "))

  NAs <- is.na(target.df[,2])
  if(any(NAs) & !na.allow) stop("Target is NA for level(s) ", toString(target_levels[NAs]), sep = ", ")
  
  ## ---- rebase targets to sample size ----
  w8margin <- target.df
  origSum <- sum(target.df$Freq, na.rm = TRUE)
  if(is.null(samplesize)) samplesize <- origSum
  checkRebaseTolerance(origSum = origSum, newSum = samplesize, rebase.tol = rebase.tol, varname = varname)
  w8margin$Freq <- (target.df$Freq / origSum) * samplesize #rebase targets to sample size
  
  ## ---- generate output object ----
  class(w8margin) <- c("w8margin", "data.frame")
  rownames(w8margin) <- NULL
  return(w8margin)
}

#' @rdname as.w8margin
#' @export
as.w8margin.numeric <- function(target, varname, levels = NULL, samplesize = NULL, na.allow = FALSE, rebase.tol = .01, ...){
  target.numeric <- target
  forcedLevels <- levels
  
  ## ---- error handling ----
  if(length(target) < 1) stop("Vectors must have a length of at least one for conversion to w8margin")
  
  if(is.null(forcedLevels)){
    if(sum(is.na(names(target.numeric))) > 0 | is.null(names(target.numeric))) stop("Vector has invalid or missing names; try specifying levels")
  } else{
    if(length(forcedLevels) != length(target.numeric)) stop("levels must be of length ", length(target.numeric))
    names(target.numeric) <- forcedLevels
  }
  duplicates <- duplicated(names(target.numeric))
  if(sum(duplicates) > 0) stop("Duplicate target level(s) ", toString(names(target.numeric[duplicates]), sep = ", "))
  
  NAs <- is.na(target.numeric)
  if(any(NAs) & !na.allow) stop("Target is NA for level(s) ", toString(names(target.numeric[NAs])), sep = ", ")
  
  ## ---- rebase targets to sample size ----
  origSum <- sum(target.numeric, na.rm = TRUE)
  if(is.null(samplesize)) samplesize <- origSum
  checkRebaseTolerance(origSum = origSum, newSum = samplesize, rebase.tol = rebase.tol, varname = varname)
  
  target.counts <- (target.numeric / origSum) * samplesize #rebase targets to sample size
  
  ## ---- generate output object ----
  w8margin <- data.frame(names(target.counts), target.counts)
  names(w8margin) <- c(varname, "Freq")
  
  class(w8margin) <- c("w8margin", "data.frame")
  rownames(w8margin) <- NULL
  return(w8margin)
}

#' @rdname as.w8margin
#' @export
as.w8margin.matrix <- function(target, varname, levels = NULL, samplesize = NULL, na.allow = FALSE, rebase.tol = .01, byrow = TRUE, ...){
  target.matrix <- target
  forcedLevels <- levels #Internally, we will use a forcedLevels parameter to avoid confusion with the levels function
  
  # Convert to vector
  target.vector <- gdata::unmatrix(target.matrix, byrow = byrow)
  
  # Then convert to numeric
  w8margin <- as.w8margin.numeric(target.vector, varname = varname, levels = forcedLevels, samplesize = samplesize, na.allow = na.allow, rebase.tol = rebase.tol)
  return(w8margin)
}



## ==== w8margin_matched ====

#TO DO:
#accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables

#' Check if w8margin Matches Observed Data
#' @description Checks whether specified \code{\link{w8margin}} object and variable in observed
#'   data are compatible, and are expected to produce valid call to
#'   \code{\link[survey]{rake}}. Returns a logical true/false, and generates
#'   warning messages to specify likely issues. Intended to help quickly
#'   diagnose incompatibilities between w8margins and observed data.
#' @usage w8margin_matched(w8margin, observed, refactor = FALSE, 
#'   na.targets.allow = FALSE, zero.targets.allow = FALSE)
#' @param w8margin w8margin object, or other object type that can be coerced to
#'   w8margin with a temporary variable name.
#' @param observed factor vector (or, if \code{refactor = TRUE}, a vector that can
#'   be coerced to factor).
#' @param refactor logical, specifying whether to factor observed variable before checking
#'   match.
#' @param na.targets.allow logical, indicating whether NA values in target should produce error (\code{FALSE}, the default) 
#'   or be allowed. NA values are never allowed in observed data.
#' @param zero.targets.allow logical, indicating whether zero values in target should produce error (\code{FALSE}, the default) 
#'   or be allowed. 
#' @return A logical, indicating whether w8margin is compatible with observed.
#' @details With default parameters (\code{na.targets.allow = FALSE}, \code{zero.targets.allow = FALSE},
#'   and \code{refactor = FALSE}), the function checks whether a \code{w8margin}
#'   object is in the strict format required by \code{\link[survey]{rake}}; this format
#'   will also be accepted by \code{\link{rakesvy}} and \code{\link{rakew8}}. Changing
#'  the default parameters relaxes some checks. With the parameters 
#'   altered, the function will only assess whether \code{w8margin} objects are 
#'   usable by \code{\link{rakesvy}} and \code{\link{rakew8}}, which
#'   accept a more flexible range of target formats.
#' @details It should not generally be necessary to call \code{w8margin_matched} manually when 
#'   using \code{\link{rakesvy}} and \code{\link{rakew8}} to compute weights.
#'   However,  may be useful to call directly, when manually calling underlying
#'   weighting functions from the \code{survey} package, or for diagnostic purposes.
#' @example inst/examples/w8margin_matched_examples.R
#' @export
w8margin_matched <- function(w8margin, observed, refactor = FALSE, na.targets.allow = FALSE, zero.targets.allow = FALSE){
  
  success <- TRUE
  
  # Check that parameterse other than w8margin and observed are valid
  if(is.na(as.logical(refactor))) stop("Invalid value for refactor; must be coercable to TRUE or FALSE")
  if(is.na(as.logical(na.targets.allow))) stop("Invalid value for na.targets.allow; must be coercable to TRUE or FALSE")
  if(is.na(as.logical(zero.targets.allow))) stop("Invalid value for zero.targets.allow; must be coercable to TRUE or FALSE")
  
  ## --- Error handling ----
  if(is.factor(observed) == FALSE){
    if(refactor == FALSE){
        warning("Observed data is not a factor variable, try using refactor = TRUE", call. = FALSE)
        success <- FALSE
    }
  }
  if(refactor == TRUE) observed <- factor(observed)
  obs_levels <- levels(observed)

  if(!("w8margin" %in% class(w8margin))){
    warning("w8margin must be an object of class w8margin, try converting using as.w8margin")
    success <- FALSE
  }
  targetname <- colnames(w8margin)[1]
  
  # Stop if one or both of these checks have failed
  if(!success) return(FALSE)
  
  ## ---- Check for NAs in observed data and target ----
  if(any(is.na(observed))){
      warning("NAs in observed data for target ", targetname, call. = FALSE)
      success <- FALSE
  }
  if(!na.targets.allow){
    if(any(is.na(w8margin[,2]))){
      warning("Target ", targetname, " is NA for level(s) ", toString(w8margin[is.na(w8margin[,2]),1], sep = ", "),  call. = FALSE)
      success <- FALSE
    }
  }
  
  # Stop if one or both of these checks have failed
  if(!success) return(FALSE)
  
  ## ---- Check for empty levels in observed and target ----
  # An empty factor levels should normally banned
  # However, we should allow empty factor levels if the target for this level is NA (and na.targets.allow = TRUE)
  # Since this means all cases with the empty level will get dropped
  emptyObserved <- table(observed) == 0 # vector of T/Fs
  if(na.targets.allow){ 
    naTargets <- w8margin[is.na(w8margin[,2]), 1]
    emptyObserved <- emptyObserved[!(names(emptyObserved) %in% naTargets)]
  }
  emptyObservedError <- any(emptyObserved)
  
  emptyTarget <- !(is.na(w8margin$Freq) | w8margin$Freq != 0) # Non-zero or empty
  emptyTargetError <- !zero.targets.allow & any(emptyTarget)
  
  if(emptyObservedError | emptyTargetError){
      if(emptyObservedError) warning("Empty factor level(s) ", paste(levels(observed)[emptyObserved], collapse = ", "), " in observed data for target ", targetname, call. = FALSE)
      if(emptyTargetError)  warning("Target ", targetname, " is zero for level(s) ", paste(w8margin[emptyTarget,1], collapse = ", "), call. = FALSE)
      return(FALSE)
  }
  
  ## ---- Check if number of levels in observed data matches length of target ----
  if(length(w8margin[,1]) != length(obs_levels)){
    warning("Number of variable levels in observed data does not match length of target ", targetname, call. = FALSE)
    matchedLength <- FALSE
    success <- FALSE
  } else matchedLength <- TRUE
  
  ## ---- Check for levels in observed data that do not match levels in target ----
  # if length doesn't match, or length match but sorted names don't
  if(!matchedLength | suppressWarnings(sum(sort(as.character(w8margin[,1])) != sort(obs_levels)) > 0)){
    #Identify missing levels in both observed and target 
    missing_from_target.index <- !(w8margin[,1] %in% obs_levels)
    missing_from_obs.index <- !(obs_levels %in% w8margin[,1])
    missing_from_target.string <- paste(w8margin[missing_from_target.index, 1], collapse = ", ")
    missing_from_obs.string <- paste(obs_levels[missing_from_obs.index], collapse = ", ")
    
    if(sum(missing_from_target.index) > 0) warning("Variable levels ", toString(missing_from_target.string, sep = ", "), " in target ", targetname, " are missing from observed factor variable", call. = FALSE)
    if(sum(missing_from_obs.index) > 0) warning("Variable levels ", toString(missing_from_obs.string, sep = ", "), " in observed factor variable are missing from target ", targetname, call. = FALSE)
    
    success <- FALSE
  }
  
  #If all checks pass, return TRUE
  if(!success) return(FALSE) else return(TRUE)
}



## ==== IMPUTE W8MARGIN ====
#'  Impute NAs in w8margin Object
#'  
#' @description Imputes NA values in a weight target (in \code{\link{w8margin}} form), based 
#'    on the observed distribution of the variable in a dataset.
#' @usage impute_w8margin(w8margin, observed, weights = NULL, rebase = TRUE)
#' @param w8margin w8margin object, with NA values that should be imputed based 
#'    on observed data.
#' @param observed factor or character vector, containing observed data used
#'    for imputing targets.
#' @param weights numeric vector of weights, the same length as \code{observed}, to
#'   be used when computing the distribution of the observed variable. NULL is
#'   equivalent to a vector where all elements are 1, and indicates the data is
#'   unweighted.
#' @param rebase logical, indicating whether non-NA weight targets should be adjusted
#'   so that the total target sample size is unchanged (\code{rebase = TRUE}), or 
#'   whether non-NA weight targets should remain the same and total target sample size
#'   increases.
#' @return A w8margin object, where NA target frequencies have been replaced using
#'   the observed distribution of the weighting variable.
#' @details Any NA target frequencies in \code{w8margin} are imputed using the 
#'   percentage distribution in \code{observed}, from \code{svytable(~observed, Ntotal = 1, ...)}.
#'   The percentage is multiplied by the desired target sample size. For example, 
#'   if has a target of NA and a desired total sample of 1500, and the 
#'   observed frequency of the weighting variable is 0%, the imputed target will 
#'   be (10% * 1500). If a \code{weights} argument is provided, then weighted 
#'   percentage distributions are used; this may be useful when design weights are 
#'   present, or when first raking on variables with complete targets.
#' @details If \code{rebase == TRUE} (the default), targets for non-NA categories 
#'   are scaled down so that the total target frequency (\code{sum(w8margin$Freq, na.rm = TRUE)})
#'   remains constant, after imputing new category targets. If \code{rebase == FALSE},
#'   targets for non-NA categories remain constant, and the total target frequency
#'   will increase.
#' @details There is an important theoretical distinction between missing \emph{targets}
#'   for conceptually valid categories, versus missing observed data due to
#'   non-response or refusal. It is only conceptually appropriate to impute targets
#'   if the targets themselves are missing. When handling missing observed data,
#'   multiple imputation techniques (such as \code{\link[mice]{mice}}) will often
#'   produce better results, except when missingness is closely related to 
#'   weighting variable ("missing not at random" in Rubin's terminology).
#' @example inst/examples/impute_w8margin_example.R
#' @references Rubin, Donald, and Roderick Hill. 2019. *Statistical Analysis with Missing*
#'   *Data, Third Edition*. New York: Wiley.
#' @export
impute_w8margin <- function(w8margin, observed, weights = NULL, rebase = TRUE){
    # ---- Check inputs ----
    if(!("w8margin" %in% class(w8margin))) stop("w8margin argument must be an object of class w8margin")
    if(!"factor" %in% class(observed)) observed <- factor(observed)
    if(is.na(as.logical(rebase))) stop("Invalid value for rebase; must be coercable to TRUE or FALSE")
  
    # ---- Impute
    obs_svy <- survey::svydesign(ids = ~1, data = data.frame(y  = observed), weights = weights)
    
    # Get variable name, and list of cats with NA target
    var_name <- names(w8margin)[names(w8margin) != "Freq"]
    na_cats <- as.character(w8margin[[var_name]][is.na(w8margin$Freq)])
    valid_cats <- as.character(w8margin[[var_name]][!is.na(w8margin$Freq)])
    valid_cats_target_sum <- sum(w8margin$Freq, na.rm = TRUE)
    
    # Check if w8margin matches observed
    # Except for
    #     a) NA targets
    #     b) Empty factor levels that are associated with NA targets 
    if(!
        (w8margin_matched(w8margin, observed, refactor = FALSE, na.targets.allow = TRUE) | 
         suppressWarnings(w8margin_matched(w8margin, observed, refactor = TRUE, na.targets.allow = TRUE)))
    ) stop("Target does not match observed data")
    
    # Generate table of observed data
    observed_table_pct <- survey::svytable(~y, design = obs_svy, Ntotal = 1)
    na_cats_obs_pct <- observed_table_pct[names(observed_table_pct) %in% na_cats]
    
    # Compute new base size
    if(rebase == TRUE){
    new_base <- valid_cats_target_sum
    } else if(rebase == FALSE){
    new_base <- valid_cats_target_sum / (1 - sum(na_cats_obs_pct))
    } else stop("rebase argument must be TRUE or FALSE")
    
    # Created imputed w8margin object
    w8margin_imputed <- w8margin
    rownames(w8margin_imputed) <- w8margin_imputed[[var_name]]
    
    # Rescale old valid categories
    w8margin_imputed[valid_cats, "Freq"] <- (w8margin_imputed[valid_cats, "Freq"] / valid_cats_target_sum) * (1 - sum(na_cats_obs_pct)) * new_base
    
    # Impute invalid categories
    w8margin_imputed[na_cats,"Freq"] <- na_cats_obs_pct[na_cats] * new_base
    
    # Return output
    rownames(w8margin_imputed) <- NULL
    return(w8margin_imputed)
}



## ==== INTERNAL FUNCTIONS ====

# ---- check target length ----
target.length <- function(x, ...){
  UseMethod("target.length")
}

target.length.numeric <- function(w8margin){length(w8margin)}
target.length.data.frame <- function(w8margin){nrow(w8margin)}
target.length.matrix <- function(w8margin){nrow(w8margin) * ncol(w8margin)}


# ---- check rebase tolerance ----

# Checks if rebased w8margin is close to either the original sample size or 1.00
# Inputs:
  # origSum: the original sample size
  # newSum: the new (rebased) sample size
  # tolerance: the tolerance, as a percent difference (eg, a 1% difference between rebased and original target)
  # varname: name of the variable, in order
# Outputs:
  # TRUE or FALSE, indicating whether the rebased target is within +-rebase.tol of the original target
  # also generates a warning if FALSE
checkRebaseTolerance <- function(origSum, newSum, rebase.tol, varname){
  ratios <- c(1, newSum) / origSum #Compute the ratio of 1 and the original sample size to OrigSum
  isTolerated <- any(ratios > (1 - rebase.tol) & ratios < (1 + rebase.tol)) #check if the ratio is 1 +- some tolerance

  if(isTolerated == FALSE) warning("original targets for variable ", varname, " sum to ", origSum, " and will be rebased")
  return(isTolerated)
}


