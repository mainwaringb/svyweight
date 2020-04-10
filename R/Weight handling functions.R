#major to-do list:

#nice to have
#as.w8target
    #1) think about as.w8target.w8target and as.w8target.array
    #2) create shared "checkTolerance" function
    #3) **allow single-column data frames with named rows (and check for other row names)**
    #4) move core of as.w8target.matrix to calling as.w8target.vector
#checktargetmatch:
    #1) accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables
## rakesvy:
    # 1) allow weightarget.id to be specified separately for each weighting variable
    # 2) Don't rename columns of data frames when converting to w8target



## ==== FUNCTIONS TO CONVERT MATRICES, DFS, AND VECTORS TO W8TARGETS ====

# "w8target" is a class I have created to specify the format required for "rake" weighting variables
# it is a data frame with varname and "Freq" columns, and named rows that list each level of the variable
# the Freq colun contains the *count* (not percent) of each level, as desired by "rake"

#TO DO: 
#create shared checkTolerance function, called by all methods
#think about as.w8target.w8target and as.w8target.array

#' Convert Matrix to w8target Object
#' 
#' @description Takes a matrix (with row and column names), and converts to a
#'   \code{w8target} object with specified name and sample size (rebasing if
#'   necessary).
#' @usage as.w8target.matrix(target, varname, samplesize = NULL, forcedLevels =
#'   NULL, byrow = TRUE, rebaseTolerance = .01)
#' @param target Matrix with row and column names, for converstion to a w8target
#'   object. By default (unless overridden by forcedLevels), the target levels
#'   of \code{w8target} will come from the interaction of row and column names.
#' @param varname Character vector specifying the name of the observed variable
#'   that the w8target object should match
#' @param samplesize  Integer with the desired target sample size for the
#'   w8target object. Defaults to \code{sum(targe)}.
#' @param forcedLevels Character vector of length \code{ncol(target) * nrow(target)} to override
#'   default target levels of w8target
#' @param byrow Logical. If FALSE, the elements within columns will be adjacent
#'   in the resulting w8target object, otherwise elements within rows will be
#'   adjacent.
#' @param rebaseTolerance Numeric betweeen 0 and 1. If targets are rebased, and
#'   the rebased sample sizes differs from the original sample size by more than
#'   this percentage, generates a warning.
#' @return An object of class w8target, with specified varname and samplesize.
as.w8target.matrix <- function(target, varname, samplesize = NULL, forcedLevels = NULL, byrow = TRUE, rebaseTolerance = .01){
  target.matrix <- target
  
  ## ---- set names for target levels ----
  target.vector <- gdata::unmatrix(target.matrix, byrow = byrow)
  if(is.null(forcedLevels)){  #set w8target levels based on row and column names, if "forcedLevels" is not specified
    if(sum(is.na(rownames(target.matrix))) > 0 | is.null(rownames(target.matrix))) stop("Matrix has invalid or missing row names")
    if(sum(is.na(colnames(target.matrix))) > 0 | is.null(colnames(target.matrix))) stop("Matrix has invalid or missing column names")
    names(target.vector) <- gsub(":", ".", names(target.vector))
  } else{  #set w8target levels based on forcedLevels input, if it us specified
    if(length(forcedLevels) != length(target.vector)) stop("forcedLevels must be of length ", length(target.vector))
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
    isTolerated <- sum(checkTolerance.vec > (1 - rebaseTolerance) & checkTolerance.vec < (1 + rebaseTolerance)) #check if the ratio is 1 +- some tolerancee
    if(isTolerated == FALSE) warning("targets for variable ", varname, " sum to ", origSum, " and were rebased")
  }
  target.counts <- (target.vector / origSum) * samplesize 
  
  ## ---- generate output object ----
  w8target <- data.frame(names(target.counts), target.counts)
  names(w8target) <- c(varname, "Freq")
  
  class(w8target) <- c("w8target", "data.frame")
  return(w8target)
}

#' Convert Data Frame to w8target Object
#' @description Takes a data frame with two columns (one named "Freq" specifying
#'   target levels names, and one specifying the target number for each level).
#'   Converts to a \code{w8target} object with specified name and sample size
#'   (rebasing if necessary).
#' @usage as.w8target.data.frame(target, varname = NULL, samplesize = NULL,
#'   forcedLevels = NULL, rebaseTolerance = .01).
#' @param target Data frame for conversion to a \code{w8target} object. Must have two
#'   variables: one numeric variable named "Freq" specifying target numbers, and
#'   one variable specifying names for each target level (unless overridden by
#'   forcedLevels).
#' @param varname Character vector specifying the name of the observed variable
#'   that the \code{w8target} object should match. If NULL, default value is the name
#'   of the data frame variable other than "Freq")
#' @param samplesize Integer with the desired target sample size for the
#'   w8target object. Defaults to \code{sum(target$Freq)}.
#' @param forcedLevels Character vector of length \code{nrow(target)} to override default
#'   target levels of w8target.
#' @param rebaseTolerance Numeric betweeen 0 and 1. Generates a warning if targets are rebased, and
#'   the rebased sample sizes differs by more than this percentage.,
#' @return An object of class w8target, with specified varname and samplesize.
as.w8target.data.frame <- function(target, varname = NULL, samplesize = NULL, forcedLevels = NULL, rebaseTolerance = .01){
  target.df <- target
  
  ## ---- error handling ----
  if(!("Freq" %in% names(target.df))) stop("Data frames must have Freq column for conversion to w8target")
  if(ncol(target.df) != 2) stop("data frames must have two columns for converstion to w8target")
  
  target_levels <- target.df[,names(target.df) != "Freq"]
  duplicates <- duplicated(target_levels)
  if(sum(duplicates) > 0) stop("Duplicated target level(s) ", toString(target_levels[duplicates], sep = ", "))

  NAs <- is.na(target.df[,2])
  if(any(NAs)) stop("Target is NA for level(s) ", toString(target_levels[NAs]), sep = ", ")
  
  ## ---- rebase targets to sample size ----
  w8target <- target.df
  origSum <- sum(target.df$Freq)
  if(is.null(samplesize)){
    samplesize <- origSum
  }else{ #generate a warning message if the original target doesn't sum to 1, 100, or samplesize (+- some tolerance)
    checkTolerance.vec <- c(1, 100, samplesize) / origSum #Compute the ratio of 1, 100, and the original sample size to OrigSum
    isTolerated <- sum(checkTolerance.vec > (1 - rebaseTolerance) & checkTolerance.vec < (1 + rebaseTolerance)) #check if the ratio is 1 +- some tolerancee
    if(isTolerated == FALSE) warning("targets for variable ", varname, " sum to ", origSum, " and were rebased")
  }
  w8target$Freq <- (target.df$Freq / origSum) * samplesize #rebase targets to sample size
  
  ## ---- generate output object ----
  if(!is.null(forcedLevels)) w8target[names(w8target) != "Freq"] <- forcedLevels
  if(!is.null(varname)) names(w8target)[names(w8target) != "Freq"] <- varname
  
  class(w8target) <- c("w8target", "data.frame")
  return(w8target)
}

#' Convert Numeric Vector to w8target Object
#' @description Takes a named numeric vector, and converts to a \code{w8target} object
#'   with specified variable name and sample size (rebasing if necessary).
#' @usage as.w8target.numeric(target, varname, samplesize = NULL, forcedLevels =
#'   NULL, rebaseTolerance = .01)
#' @param target Named vector, for converstion to a w8target object. By default
#'   (unless overridden by \code{forcedLevels}), the target levels of w8target will
#'   come from the names of the vector.
#' @param varname Character vector specifying the name of the observed variable
#'   that the w8target object should match.
#' @param samplesize Integer with the desired target sample size for the
#'   w8target object. Defaults to \code{sum(target)}.
#' @param forcedLevels Character vector of length \code{length(target)} to override default
#'   target levels of w8target.
#' @param rebaseTolerance Numeric betweeen 0 and 1. If targets are rebased, and
#'   the rebased sample sizes differs from the original sample size by more than
#'   this percentage, generates a warning.
#' @return An object of class w8target, with specified varname and samplesize.
as.w8target.numeric <- function(target, varname, samplesize = NULL, forcedLevels = NULL, rebaseTolerance = .01){
  target.numeric <- target
  
  ## ---- error handling ----
  if(is.null(forcedLevels)){
    if(sum(is.na(names(target.numeric))) > 0 | is.null(names(target.numeric))) stop("Vector has invalid or missing names; try specifying forcedLevels")
  } else{
    if(length(forcedLevels) != length(target.numeric)) stop("forcedLevels must be of length", length(target.numeric))
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
    isTolerated <- sum(checkTolerance.vec > (1 - rebaseTolerance) & checkTolerance.vec < (1 + rebaseTolerance)) #check if the ratio is 1 +- some tolerancee
    if(isTolerated == FALSE) warning("Targets for variable ", varname, " sum to ", origSum, " and were rebased")
  }
  target.counts <- (target.numeric / origSum) * samplesize #rebase targets to sample size
  
  ## ---- generate output object ----
  w8target <- data.frame(names(target.counts), target.counts)
  names(w8target) <- c(varname, "Freq")
  
  class(w8target) <- c("w8target", "data.frame")
  return(w8target)
}

as.w8target <- function(x, ...){
  UseMethod("as.w8target")
}



## ==== CHECKTARGETMATCH ====

#TO DO:
#accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables

#' Check Whether w8target Object Matches Observed Variable
#' @description Checks whether specified \code{w8target} object and observedVar are
#'   compatible, and are expected to produce valid call to rake. Returns a
#'   logical true/false, and generates warning messages to specify likely issue.
#'   Intended to help quickly diagnose incompatibilities between w8targets and
#'   observed data.
#' @usage checkTargetMatch(w8target, observedVar, exact = FALSE, refactor =
#'   FALSE)
#' @param w8target w8target object (or data frame that in the format specified
#'   by rake, which can behave as a w8target object).
#' @param observedVar factor variable (or, if refactor = FALSE, a variable that
#'   can be coerced to factor).
#' @param refactor logial, specifying whether to factor variable before checking
#'   match.
#' @param exactMatch logical, specifying if levels in w8target must be in the
#'   same order as factor levels in observedVar.
#' @return A logical, indicating whether w8target is compatible with observedVar.
checkTargetMatch <- function(w8target, observedVar, exactMatch = FALSE, refactor = FALSE){
  
  ## --- Error handling ----
  if(is.factor(observedVar) == FALSE){
    if(refactor == FALSE){
        warning("observed data is not a factor variable")
        return(FALSE)
    }
  }
  if(refactor == TRUE) observedVar <- factor(observedVar)
  obs_levels <- levels(observedVar)
  
  if(!("w8target" %in% class(w8target))){
    w8target <- as.w8target(w8target, varname = "(unnamed target)")
  }
  targetname <- colnames(w8target)[1]
  
  ## ---- Check for NAs in observed data and target ----
  if(any(is.na(observedVar))){
      warning("NAs in observed data for ", targetname)
      return(FALSE)
  }
  if(any(is.na(w8target[,2]))){
      warning("Target is NA for levels() ", toString(w8target[is.na(w8target[,2]),1], sep = ", "), " on variable ", targetname)
      return(FALSE)
  }
  
  ## ---- Check for empty levels in observedVar and target ----
  emptyObserved <- table(observedVar) == 0
  hasEmptyObserved <- sum(emptyObserved)
  emptyTarget <- w8target$Freq == 0
  hasEmptyTarget <- sum(emptyTarget)
  
  if(hasEmptyObserved > 0 | hasEmptyTarget > 0){
      if(hasEmptyObserved > 0) warning("Observed data for ", targetname, " contains empty factor level ", paste(levels(observedVar)[emptyObserved], collapse = ", "))
      if(hasEmptyTarget > 0)  warning("Weight target ", targetname, " contains empty factor level ", paste(obs_levels[emptyTarget], collapse = ", "))
      return(FALSE)
  }
  
  
  ## ---- Check if number of levels in observed data matches length of target ----
  if(length(w8target[,1]) != length(obs_levels)){
    warning("Number of variable levels in observed data does not match length of target ", targetname)
    return(FALSE)
  }
  
  ## ---- Check for levels in observed data that do not match levels in target ----
  
  #If exactMatch == TRUE, check if unsorted levels are the same in target and observed
  if(exactMatch == TRUE & (sum(w8target[,1] != obs_levels) > 0) & (sum(sort(w8target[,1]) != sort(obs_levels)) == 0)){
    warning("Variable levels in target", targetname, " are in different order from observed factor variable")
    return(FALSE)
  }
  #otherwise, check if *sorted* variable levels are the same
  if(sum(sort(as.character(w8target[,1])) != sort(obs_levels)) > 0){
    #Identify missing levels in both observed and target 
    missing_from_target.index <- !(w8target[,1] %in% obs_levels)
    missing_from_obs.index <- !(obs_levels %in% w8target[,1])
    missing_from_target.string <- paste(w8target[missing_from_target.index, 1], collapse = ", ")
    missing_from_obs.string <- paste(obs_levels[missing_from_obs.index], collapse = ", ")
    
    if(sum(missing_from_target.index) > 0) warning("variable levels ", toString(missing_from_target.string, sep = ", "), " in target ", targetname, " are missing from observed factor variable")
    if(sum(missing_from_obs.index) > 0) warning("variable levels ", toString(missing_from_obs.string, sep = ", "), " in observed factor variable are missing from target ", targetname)
    
    return(FALSE)
  }
    
  #If all checks pass, return TRUE
  return(TRUE)
}



## ==== RAKESVY + RAKEW8 ====

#This is the workhorse function - a wrapper for "rake" that is intended to take
#targets in a more flexible format However, flexibility also can be dangerous!

#TO DO
#Don't rename columns of data frames when converting to w8target
#allow weightarget.id to be specified separately for each weighting variable

#' Flexibly Calculate Rake Weights
#' @description Calculates rake weights, using flexible flexible syntax for data
#'   and weight target specification. Runs pre-processing, then calls
#'   \code{\link[survey]{rake}} to comptue weights. rakesvy returns a
#'   weighted \code{svydesign} object, while rakew8 returns a vector of
#'   weights.
#' @usage rakesvy(design, weightTargets, samplesize = "fromData", matchLevelsBy
#'   = "name", matchVarsBy = "listname", rebaseTolerance = .01,
#'   ...)
#' @param design An \code{\link[survey]{svydesign}} object, or a data frame that
#'   can be coerced to an svydesign object (assuming no clustering or design
#'   weighting)
#' @param weightTargets A list of w8target objects, or other objects that can be
#'   coerced to weightTargets.
#' @param samplesize Either an integer specifying the desired post-raking sample
#'   size, or a character string "fromData" or "fromTargets" specfiying how to
#'   calculate the desired sample size (see details).
#' @param matchLevelsBy A character string that specifies how to match levels in
#'   the target with the observed data, either "name" (the default) or "order"
#'   (see details).
#' @param matchVarsBy A character  string that specificies how elements of
#'   weightTargets are matched with variables in design, either "listname" (the
#'   default) or "colname" (see details).
#' @param rebaseTolerance Numeric betweeen 0 and 1. If targets are rebased, and
#'   the rebased sample sizes differs from the original sample size by more than
#'   this percentage, generates a warning.
#' @details rakesvy and rakew8 are a wrapper for \code{\link[survey]{rake}} that wrangles
#'   observed data and targets. It cleans matches weight targets to observed
#'   variables, cleans both targets and observed varaibles, and then checks the
#'   validity of weight targets (partially by calling
#'   \code{\link{checkTargetMatch}} before raking. It also allows a weight
#'   target of zero, assigns an automatic weight of zero to cases on this target
#'   level.
#' @details Weight target levels can be matched with observed variable levels in
#'   two ways, specified via the \code{matchLevelsBy} parameter. "name" (the
#'   default) matches based on name, disregarding order (so a "male" level in
#'   the weight target will be matched with a "male" level in the observed
#'   data). "order" matches based on order, disregarding name (so the first
#'   level or row of the target will match with the first level of the observed
#'   factor variable).
#' @details Weight targets can also be matched to observed variables in two
#'   ways, specified via the \code{matchVarsBy} paramter. The default,
#'   "listname", indicated that the names of elements in the list weightTargets
#'   should indicate variables in the design object. The alternative, "colname"
#'   specifies that the non-"Freq" column name of each item in the list
#'   weightTargets should indicate a matching variable in the design object;
#'   this will only work for weight targets in a \code{w8target} or
#'   \code{data.frame} format.
#' @details The desired sample size (in other words, the desired sum of weights
#'   after raking)  is specifeid via the \code{samplesize} parameter. This can
#'   be a numeric value. Alternatively, "fromData" specifies that the observed
#'   sample size before weighting (taken from \code{sum(weights(design))} if
#'   appliable, or \code{nrow} if not); "fromTargets" specifies that the total
#'   sample sizes in target objects should be followed, and should only be used
#'   if all targets specify the same sample size.
#' @return rakesvy resutns an \code{svydesign} object with rake weights applied. Any changes
#'   made the variables in \code{design} in order to call \code{rake}, such as
#'   dropping empty factor levels, are temporary and \emph{not} returned in the
#'   output object. 
#' @return rakew8 returns a vector of eights. This avoids creating
#'   duplicated svydesign objects, which can be useful when calculating multiple
#'   sets of weights for the same data.
rakesvy <- function(design, weightTargets, samplesize = "fromData", matchLevelsBy = "name", matchVarsBy = "listname", rebaseTolerance = .01, ...){
    w8 <- rakew8(design = design, weightTargets = weightTargets, samplesize = samplesize, 
                 matchLevelsBy = matchLevelsBy, matchVarsBy = matchVarsBy, rebaseTolerance = rebaseTolerance, ...)
    design$weights <- w8
    
    return(design)
}

#' @describeIn rakesvy Calculate a vector of raked weights
rakew8 <- function(design, weightTargets, samplesize = "fromData", matchLevelsBy = "name", matchVarsBy = "listname", rebaseTolerance = .01, ...){

  ## ==== HOUSEKEEPING ====
    
  # ---- Check for valid values on inputs ----
  if(sum(!(matchLevelsBy %in% c("name", "order", "exact"))) > 0) stop("Invalid value(s) ", paste(matchLevelsBy[!(matchLevelsBy %in% c("name", "order", "exact"))])," in matchLevelsBy")
  if(sum(!(matchVarsBy %in% c("colname", "listname"))) > 0) stop("Invalid value(s) ", paste(matchVarsBy[!(matchVarsBy %in% c("colname", "listname"))])," in matchVarsBy")
  
  
  # ---- Convert misc objects to needed classes ----
  # Convert data frame to svydesign object
  if("data.frame" %in% class(design)){
      #Notice that we are suppressing the warning here - svydesign will otherwise produce a warning that no input weights are provided
      suppressWarnings(design <- survey::svydesign(~0, data = design, control = list(verbose = FALSE)))
  } 
    
  # Define sample size 
  if(samplesize == "fromData"){ #"fromData" means we want to take a centrally-specified sample size
    samplesize <- sum(weights(design))
  } else if(samplesize == "fromTargets"){ #"fromTargets" means we want to take the sample size found in the targets, IE not specify one here
      samplesize <- NULL
  }
    
  #If a single vector/dataframe/matrix/weighttarget 
  if(!("list" %in% class(weightTargets))) weightTargets <- list(weightTargets)
  
  # if matchLevelsBy is a scalar, repeat it for every variable
  if(length(matchLevelsBy) == 1) matchLevelsBy <- rep(matchLevelsBy, length(weightTargets))
  else if(length(matchLevelsBy) != length(weightTargets)) stop("incorrect length for matchLevelsBy")
  
    
  
  ## ==== IDENTIFY NAMES OF WEIGHTING VARIABLES ====
    
  #Names of weighting variables can be contained in one of two places:
  # A) name of item in the weightTarget list (preferable), applicable even if we use as.w8target to convert target types
  # B) the name of the second column of a w8target object, applicable only if targets are class w8target or data frame
  which.w8target <- sapply(weightTargets, function(x) "w8target" %in% class(x))
  
  if(matchVarsBy == "listname"){
    weight_target_names <- names(weightTargets) #set weight_target_names convenience variables to equal the list names
    if(length(unique(weight_target_names)) < length(weightTargets)){
      if(is.null(weight_target_names)) stop("List of weight targets must be named unless matchVarsBy is set to 'colnames'")
      if(sum(weight_target_names == "") > 0) stop("One or more weight target names is blank")
      stop("Duplicated weight targets names", paste(weight_target_names[duplicated(weight_target_names)], sep = ", " ))
    }
   
    old_column_names <- lapply(weightTargets[which.w8target], function(w8target) colnames(w8target)[1])
    weightTargets[which.w8target] <- mapply(function(w8target, varname){ #for any targets that were originally in w8target format: change column name to match list name, and generate a warning
      if(colnames(w8target)[1] != varname){
        colnames(w8target)[1] <- varname
      }
      return(w8target)
    }, w8target = weightTargets[which.w8target], varname = weight_target_names[which.w8target], SIMPLIFY = FALSE)
    doesNotMatch <- weight_target_names[which.w8target] != old_column_names
    if(any(doesNotMatch)) warning("w8target column name(s) ", paste(old_column_names[doesNotMatch], collapse = ", "), " do not match list name(s) ",  paste0(weight_target_names[which.w8target][doesNotMatch], collapse = ","), "; coercing to match list name")
    
  }else if(matchVarsBy == "colname"){
    if(any(!which.w8target)) stop("matchVarsBy = 'colname' requires targets of class w8target")
      
    weight_target_names <- sapply(weightTargets, function(onetarget) names(onetarget)[1])
    doesNotMatch <- names(weightTargets) != weight_target_names
    if(any(doesNotMatch)) warning("w8target column name(s) ", paste(weight_target_names[doesNotMatch], collapse = ", "), " do not match list name(s) ",  paste0(names(weight_target_names)[doesNotMatch], collapse = ", "), "; coercing to match column name")

    names(weightTargets) <- weight_target_names
  }
  
  
  
  ## ==== PROCESS TARGETS ====
  
  #Check if target exists for weighting variables
  missing_from_observed <- !(weight_target_names %in% names(design$variables))
  if(sum(missing_from_observed) > 0) stop(paste("Observed data was not found for weighting variables ", toString(weight_target_names[missing_from_observed], sep = ", ")))
  
  # ---- force changes in target level names, if necessary ----

  #Change levels of target, for variables where matchTargetBy = "order" (this tells us that the first row of the target should automatically be the first level of the variable, and so on)
  forcedTargetLevels <- mapply(function(var, forceType){
    if(forceType == "order"){ forcedTargetLevels <- levels(var)} else forcedTargetLevels <- NULL
  }, var = design$variables[,weight_target_names, drop = FALSE], forceType = matchLevelsBy)
  
  # ---- Convert targets to class w8target ----
  #First save original samples sizes (for diagostics later) 
  origTargetSums <- lapply(weightTargets, function(x) sum(as.w8target(x, varname = "x")$Freq))
  
  #Then compute actually weight targets
  weightTargets <- mapply(as.w8target,
                                             target = weightTargets, varname = weight_target_names, forcedLevels = forcedTargetLevels,
                                             MoreArgs = list(samplesize = samplesize),
                                             SIMPLIFY = FALSE)
  
  
  
  ## ==== HANDLE ZERO WEIGHTSS ====
  
  #remove levels with target of "0" from observed data and target ----
  #survey's "rake" command will not accept a target of zero, so we need to manually drop it from a data frame and w8target object
  design$variables[,weight_target_names] <- lapply(design$variables[, weight_target_names, drop = FALSE], as.factor)
  
  # ---- Identify target levels of zero, and remove them from the targets ----
  zeroTargetLevels <- lapply(weightTargets, function(onetarget) as.character(onetarget[!is.na(onetarget$Freq) & onetarget$Freq == 0, 1])) #identify zero levels
  weightTargets <- lapply(weightTargets, function(onetarget) onetarget[is.na(onetarget$Freq) | onetarget$Freq != 0, ]) #drop zero levels from targets
  
  # ---- Remove cases (and factor levels) associated with zero targets from the data ---
  #Identify cases that have a zero target on at least one variable
  keepIndex.df <- data.frame(index = 1:nrow(design$variables), keepYN =
     rowSums(simplify2array(mapply(function(var, levelsToDrop, varname){
         if(length(levelsToDrop) > 0){
             factorLevels <- levels(as.factor(var))
             isValidLevel <- levelsToDrop %in% factorLevels
             if(any(!isValidLevel)) warning("Empty target level(s) ", toString(levelsToDrop[!isValidLevel], sep = ", "), " do not match with any observed data on variable ", varname) 
             var %in% levelsToDrop
         }
         else rep(FALSE, length(var))
     }, var = design$variables[weight_target_names], levelsToDrop = zeroTargetLevels, varname = weight_target_names))) == 0
  )
  #Identify casess that have a design weight of zero
  keepIndex.df$keeepYN[weights(design) == 0] <- FALSE
      
  #Check which factor levels have valid cases, before dropping cases
  predrop.tab <- lapply(design$variables[weight_target_names], table)

  #drop cases
  design <- subset(design, keepIndex.df$keepYN)
  
  #remove the unneeded factor levels
  design$variables[weight_target_names] <- mapply(function(factorvar, levelsToDrop){
      if(length(levelsToDrop) > 0) factor(factorvar, levels = levels(factorvar)[!(levels(factorvar) %in% levelsToDrop)])
      else factorvar
  }, factorvar = design$variables[weight_target_names], levelsToDrop = zeroTargetLevels, SIMPLIFY = FALSE)
  
  #Check if we are accidentally losing all cases (on factor levels with valid targets) by dropping some casses
  postdrop.tab <- lapply(design$variables[weight_target_names], table) #Table after dropping cases
  mapply(function(pre, post, levelsToDrop, varname){   #Compare tabkes before and after dropping
      pre <- pre[!(names(pre) %in% levelsToDrop)]
      post <- post[!(names(pre) %in% levelsToDrop)]
      
      lostAllCases <- post == 0 & pre != 0
      if(any(lostAllCases)) warning("All valid cases for ", varname, " level(s) ", toString(names(pre)[levelsToDrop]), " had weight zero and were dropped")
  }, pre = predrop.tab, post = postdrop.tab, levelsToDrop = zeroTargetLevels, varname = weight_target_names)
  
  
  
  ## ==== CHECK THAT TARGETS ARE VALID ====
  
  #Check if targets currently match
  isTargetMatch <- mapply(checkTargetMatch, w8target = weightTargets, observedVar = design$variables[, weight_target_names, drop = FALSE],
                          exactMatch = (matchLevelsBy == "exact"))
  #Check if targets would match after re-factoring (re-factoring might produce less helpful messages)
  suppressWarnings(isRefactoredMatch <- mapply(checkTargetMatch, w8target = weightTargets, observedVar = design$variables[, weight_target_names, drop = FALSE],
                                               exactMatch = (matchLevelsBy == "exact"), refactor = TRUE))
  #Solve issues that can be solved with refactoring, stop if refactoring can't solve issues
  if(any(!isRefactoredMatch)) stop("Target does not match observed data on variable(s) ", paste(weight_target_names[!isTargetMatch], collapse = ", "))
  else if(any(!isTargetMatch))  design$variables[,weight_target_names][!isTargetMatch] <- lapply(design$variables[, weight_target_names, drop = FALSE][!isTargetMatch], factor)
  
  
  
  ## ==== CHECK FOR CONSISTENT SAMPLE SIZES ====
  
  #to handle objects that were *not* coerced to a set sample size, check that samplesize for each w8target is the same
  finalTargetSums <- sapply(weightTargets, function(x) sum(x$Freq))
  isSizeTolerated <- (finalTargetSums / samplesize) < (1 + rebaseTolerance) & (finalTargetSums / samplesize) > (1 - rebaseTolerance)
  if(any(isSizeTolerated == FALSE)) stop("Target sample sizes vary by more than specified tolerance; try changing rebaseTolerance")

  #to handle objects that were coerced to a set sample size, check if any of the sample sizes required substantive rebasing
  rebaseRatio <- lapply(origTargetSums, function(origSum) c(1, 100, samplesize) / origSum)
  #Compute the ratio of 1, 100, and the original sample size to OrigSum
  isRebaseTolerated <- sapply(rebaseRatio, function(ratio) any((ratio > (1 - rebaseTolerance)) & (ratio < (1 + rebaseTolerance)))) #check if the ratio is 1 +- some tolerancee
  if(any(isRebaseTolerated == FALSE)) warning("targets for variable ", toString(names(weightTargets)[!isRebaseTolerated], sep = ", "), " sum to ", toString(origTargetSums[!isRebaseTolerated], sep = ", "), " and were rebased")
  
  
  
  ## ==== RUN WEIGHTS ====
  
  sample.margins <- lapply((paste0("~", weight_target_names)), as.formula)
  population.margins <- weightTargets
  
  weighted <- survey::rake(design = design, sample.margins = sample.margins, population.margins = population.margins, ...)
  
  keepIndex.df$weight <- 0
  keepIndex.df$weight[keepIndex.df$keepYN == TRUE] <- weights(weighted)
  
  return(keepIndex.df$weight)

}



#===MISCELLANEOUS FUNCTIONS===

#' Kish's approximate weighting efficiency
eff_n <- function(design){
  myweights <- survey::weights(design)
  eff_n <- (sum(myweights) ^ 2) / (sum(myweights ^ 2))
  return(eff_n)
}


