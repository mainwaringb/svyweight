#major to-do list:

#nice to have
#as.w8margin
    #1) think about as.w8margin.w8margin and as.w8margin.array
    #2) create shared "checkTolerance" function
    #3) **allow single-column data frames with named rows (and check for other row names)**
    #4) move core of as.w8margin.matrix to calling as.w8margin.vector
#target.matched:
    #1) accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables
## rakesvy:
    # 1) allow weightarget.id to be specified separately for each weighting variable
    # 2) Don't rename columns of data frames when converting to w8margin



## ==== FUNCTIONS TO CONVERT MATRICES, DFS, AND VECTORS TO w8marginS ====

# "w8margin" is a class I have created to specify the format required for "rake" weighting variables
# it is a data frame with varname and "Freq" columns, and named rows that list each level of the variable
# the Freq colun contains the *count* (not percent) of each level, as desired by "rake"

#TO DO: 
#create shared checkTolerance function, called by all methods
#think about as.w8margin.w8margin and as.w8margin.array

#' Convert Matrix to w8margin Object
#' 
#' @description Takes a matrix (with row and column names), and converts to a
#'   \code{w8margin} object with specified name and sample size (rebasing if
#'   necessary).
#' @usage as.w8margin.matrix(target, varname, levels = NULL, samplesize = NULL,
#'   byrow = TRUE, rebase.tol = .01)
#' @param target Matrix with row and column names, for conversion to a w8margin
#'   object. By default (unless overridden by levels), the target levels
#'   of \code{w8margin} will come from the interaction of row and column names.
#' @param varname Character vector specifying the name of the observed variable
#'   that the w8margin object should match.
#' @param levels Character vector of length \code{ncol(target) * nrow(target)} to override
#'   default target levels of w8margin.
#' @param samplesize  Numeric with the desired target sample size for the
#'   w8margin object. Defaults to \code{sum(targe)}.
#' @param byrow Logical. If FALSE, the elements within columns will be adjacent
#'   in the resulting w8margin object, otherwise elements within rows will be
#'   adjacent.
#' @param rebase.tol Numeric betweeen 0 and 1. If targets are rebased, and
#'   the rebased sample sizes differs from the original sample size by more than
#'   this percentage, generates a warning.
#' @return An object of class w8margin, with specified varname and samplesize.
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

#' Convert Data Frame to w8margin Object
#' @description Takes a data frame (with one or two columns, see "target" param
#'   below). Converts to a \code{w8margin} object with specified name and sample
#'   size (rebasing if necessary).
#' @usage as.w8margin.data.frame(target, varname = NULL, levels = NULL, 
#'  samplesize = NULL, rebase.tol = .01).
#' @param target Data frame for conversion to a \code{w8margin} object. Must
#'   have one numeric column specifying target values foreach level, and either
#'   a non-numeric column or row names specifying the name of each target level.
#' @param varname Character vector specifying the name of the observed variable
#'   that the \code{w8margin} object should match. Optional for two-column data
#'   frames; if omitted, If NULL, default value is the column name of the
#'   non-numeric column).
#' @param samplesize Number with the desired target sample size for the w8margin
#'   object. Defaults to \code{sum(target$Freq)}.
#' @param levels Character vector of length \code{nrow(target)} to
#'   override default target levels of w8margin.
#' @param rebase.tol Numeric betweeen 0 and 1. Generates a warning if
#'   targets are rebased, and the rebased sample sizes differs by more than this
#'   percentage.
#' @return An object of class w8margin, with specified varname and samplesize.
#' @export
as.w8margin.data.frame <- function(target, varname = NULL, levels = NULL, samplesize = NULL, rebase.tol = .01){
  target.df <- target
  forcedLevels <- levels
  
  ## ---- error handling ----
  if(ncol(target.df) > 2 | ncol(target.df) == 0) stop("Data frames must have one or two columns for conversion to w8margin")

  if(ncol(target.df) == 1){#If data frame has one column (Freq) and row names, convert row names into column
      if(all(rownames(test) == 1:nrow(target.df)) & is.null(forcedLevels)) stop("One-column data frames must have non-default row names for conversion to w8margin, unless levels are specified")
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

#' Convert Numeric Vector to w8margin Object
#' @description Takes a named numeric vector, and converts to a \code{w8margin} object
#'   with specified variable name and sample size (rebasing if necessary).
#' @usage as.w8margin.numeric(target, varname, levels = NULL, samplesize =
#'   NULL, rebase.tol = .01)
#' @param target Named vector, for conversion to a w8margin object. By default
#'   (unless overridden by \code{levels}), the target levels of w8margin will
#'   come from the names of the vector.
#' @param varname Character vector specifying the name of the observed variable
#'   that the w8margin object should match.
#'  @param levels Character vector of length \code{length(target)} to override default
#'   target levels of w8margin.
#' @param samplesize Integer with the desired target sample size for the
#'   w8margin object. Defaults to \code{sum(target)}.
#' @param rebase.tol Numeric betweeen 0 and 1. If targets are rebased, and
#'   the rebased sample sizes differs from the original sample size by more than
#'   this percentage, generates a warning.
#' @return An object of class w8margin, with specified varname and samplesize.
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

#' @export
as.w8margin <- function(x, ...){
  UseMethod("as.w8margin")
}



## ==== TARGET.MATCHED ====

#TO DO:
#accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables

#' Check Whether w8margin Object Matches Observed Variable
#' @description Checks whether specified \code{w8margin} object and observed are
#'   compatible, and are expected to produce valid call to rake. Returns a
#'   logical true/false, and generates warning messages to specify likely issue.
#'   Intended to help quickly diagnose incompatibilities between w8margins and
#'   observed data.
#' @usage target.matched(w8margin, observed, exact = FALSE, refactor =
#'   FALSE)
#' @param w8margin w8margin object (or data frame that in the format specified
#'   by rake, which can behave as a w8margin object).
#' @param observed factor variable (or, if refactor = FALSE, a variable that
#'   can be coerced to factor).
#' @param refactor logial, specifying whether to factor variable before checking
#'   match.
#' @return A logical, indicating whether w8margin is compatible with observed.
#' @export
target.matched <- function(w8margin, observed, refactor = FALSE){
  
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
      if(hasEmptyTarget > 0)  warning("Weight target ", targetname, " contains empty factor level ", paste(obs_levels[emptyTarget], collapse = ", "))
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



## ==== RAKESVY + RAKEW8 ====

#This is the workhorse function - a wrapper for "rake" that is intended to take
#targets in a more flexible format However, flexibility also can be dangerous!

#TO DO
#Don't rename columns of data frames when converting to w8margin
#allow weightarget.id to be specified separately for each weighting variable

#' Flexibly Calculate Rake Weights
#' @description Calculate rake weights on a data frame or
#'   \code{\link[survey]{svydesign}} object. Targets may be counts or
#'   percentages, in vector, matrix, data frame, or w8margin form. Before
#'   weighting, targets are converted to w8margins, checked for validity, and
#'   matched to variables in observed data, \code{rakesvy} returns a weighted
#'   \code{svydesign} object, while \code{rakew8} returns a vector of weights.
#' @usage rakesvy(design, targets, samplesize = "from.data", 
#'   match.levels.by = "name", match.vars.by = "listname", rebase.tol = .01, ...)
#' @usage rakew8(design, targets, samplesize = "from.data",
#'   match.levels.by = "name", match.vars.by = "listname", rebase.tol = .01, ...)
#' @param design An \code{\link[survey]{svydesign}} object, or a data frame that
#'   can be coerced to an svydesign object. When a data frame is coerced, the coercion assuming no clustering or design
#' weighting.
#' @param targets A list of weight targets, in a form that can be coerced
#'   to class w8margin (see \code{\link{as.w8margin}}). This includes named
#'   numeric vectors and matrices, and data frames in the format accepted by
#'   \code{rake}.
#' @param samplesize Either a number specifying the desired post-raking sample
#'   size, or a character string "from.data" or "from.targets" specfiying how to
#'   calculate the desired sample size (see details).
#' @param match.levels.by A character string that specifies how to match levels in
#'   the target with the observed data, either "name" (the default) or "order"
#'   (see details).
#' @param match.vars.by A character  string that specificies how elements of
#'   targets are matched with variables in design, either "listname" (the
#'   default) or "colname" (see details).
#' @param rebase.tol Numeric betweeen 0 and 1. If targets are rebased, and
#'   the rebased sample sizes differs from the original sample size by more than
#'   this percentage, generates a warning.
#' @details rakesvy and rakew8 are a wrapper for \code{\link[survey]{rake}} that wrangles
#'   observed data and targets. It matches weight targets to observed
#'   variables, cleans both targets and observed varaibles, and then checks the
#'   validity of weight targets (partially by calling
#'   \code{\link{target.matched}} before raking. It also allows a weight
#'   target of zero, and assigns an automatic weight of zero to cases on this target
#'   level.
#' @details Weight target levels can be matched with observed variable levels in
#'   two ways, specified via the \code{match.levels.by} parameter. "name" (the
#'   default) matches based on name, disregarding order (so a "male" level in
#'   the weight target will be matched with a "male" level in the observed
#'   data). "order" matches based on order, disregarding name (so the first
#'   level or row of the target will match with the first level of the observed
#'   factor variable).
#' @details Weight targets can also be matched to observed variables in two
#'   ways, specified via the \code{match.vars.by} paramter. The default,
#'   "listname", indicated that the names of elements in the list targets
#'   should indicate variables in the design object. The alternative, "colname"
#'   specifies that the non-"Freq" column name of each item in the list
#'   targets should indicate a matching variable in the design object;
#'   this will only work for weight targets in a \code{w8margin} or
#'   \code{data.frame} format.
#' @details The desired sample size (in other words, the desired sum of weights
#'   after raking)  is specifeid via the \code{samplesize} parameter. This can
#'   be a numeric value. Alternatively, "from.data" specifies that the observed
#'   sample size before weighting (taken from \code{sum(weights(design))} if
#'   appliable, or \code{nrow} if not); "from.targets" specifies that the total
#'   sample sizes in target objects should be followed, and should only be used
#'   if all targets specify the same sample size.
#' @return \code{rakesvy} returns an \code{svydesign} object with rake weights applied. Any changes
#'   made the variables in \code{design} in order to call \code{rake}, such as
#'   dropping empty factor levels, are temporary and \emph{not} returned in the
#'   output object. 
#' @return \code{rakew8} returns a vector of weights. This avoids creating
#'   duplicated \code{svydesign} objects, which can be useful when calculating multiple
#'   sets of weights for the same data.
#' @export
rakesvy <- function(design, targets, samplesize = "from.data", match.levels.by = "name", match.vars.by = "listname", rebase.tol = .01, ...){
    if("data.frame" %in% class(design)){
        #Notice that we are suppressing the warning here - svydesign will otherwise produce a warning that no input weights are provided
        suppressWarnings(design <- survey::svydesign(~0, data = design, control = list(verbose = FALSE)))
    } 
    
    w8 <- rakew8(design = design, targets = targets, samplesize = samplesize, 
                 match.levels.by = match.levels.by, match.vars.by = match.vars.by, rebase.tol = rebase.tol, ...)
    design$prob <- 1/w8
    
    return(design)
}

#' @rdname rakesvy
#' @export
rakew8 <- function(design, targets, samplesize = "from.data", match.levels.by = "name", match.vars.by = "listname", rebase.tol = .01, ...){

  ## ==== HOUSEKEEPING ====
    
  # ---- Check for valid values on inputs ----
  if(sum(!(match.levels.by %in% c("name", "order", "exact"))) > 0) stop("Invalid value(s) ", paste(match.levels.by[!(match.levels.by %in% c("name", "order", "exact"))])," in match.levels.by")
  if(sum(!(match.vars.by %in% c("colname", "listname"))) > 0) stop("Invalid value(s) ", paste(match.vars.by[!(match.vars.by %in% c("colname", "listname"))])," in match.vars.by")
  
  
  # ---- Convert misc objects to needed classes ----
  # Convert data frame to svydesign object
  if("data.frame" %in% class(design)){
      #Notice that we are suppressing the warning here - svydesign will otherwise produce a warning that no input weights are provided
      suppressWarnings(design <- survey::svydesign(~0, data = design, control = list(verbose = FALSE)))
  } 
    
  # Define sample size 
  if(samplesize == "from.data"){ #"from.data" means we want to take a centrally-specified sample size
    samplesize <- sum(weights(design))
  } else if(samplesize == "from.targets"){ #"from.targets" means we want to take the sample size found in the targets, IE not specify one here
      samplesize <- NULL
  }
    
  #If targets is a single vector/dataframe/matrix/weighttarget, convert to a list
  if(!("list" %in% class(targets))) targets <- list(targets)
  
  # if match.levels.by is a scalar, repeat it for every variable
  if(length(match.levels.by) == 1) match.levels.by <- rep(match.levels.by, length(targets))
  else if(length(match.levels.by) != length(targets)) stop("incorrect length for match.levels.by")
  
    
  
  ## ==== IDENTIFY NAMES OF WEIGHTING VARIABLES ====
    
  #Names of weighting variables can be contained in one of two places:
  # A) name of item in the weightTarget list (preferable), applicable even if we use as.w8margin to convert target types
  # B) the name of the second column of a w8margin object, applicable only if targets are class w8margin or data frame
  isw8margin <- sapply(targets, function(x) "w8margin" %in% class(x))
  
  if(match.vars.by == "listname"){
    weightTargetNames <- names(targets) #set weightTargetNames convenience variables to equal the list names
    if(length(unique(weightTargetNames)) < length(targets)){
      if(is.null(weightTargetNames)) stop("List of weight targets must be named unless match.vars.by is set to 'colnames'")
      if(sum(weightTargetNames == "") > 0) stop("One or more weight target names is blank")
      stop("Duplicated weight targets names", paste(weightTargetNames[duplicated(weightTargetNames)], sep = ", " ))
    }
   
    old_column_names <- lapply(targets[isw8margin], function(w8margin) colnames(w8margin)[1])
    targets[isw8margin] <- mapply(function(w8margin, varname){ #for any targets that were originally in w8margin format: change column name to match list name, and generate a warning
      if(colnames(w8margin)[1] != varname){
        colnames(w8margin)[1] <- varname
      }
      return(w8margin)
    }, w8margin = targets[isw8margin], varname = weightTargetNames[isw8margin], SIMPLIFY = FALSE)
    doesNotMatch <- weightTargetNames[isw8margin] != old_column_names
    if(any(doesNotMatch)) warning("w8margin column name(s) ", paste(old_column_names[doesNotMatch], collapse = ", "), " do not match list name(s) ",  paste0(weightTargetNames[isw8margin][doesNotMatch], collapse = ","), "; coercing to match list name")
    
  }else if(match.vars.by == "colname"){
    if(any(!isw8margin)) stop("match.vars.by = 'colname' requires targets of class w8margin")
      
    weightTargetNames <- sapply(targets, function(onetarget) names(onetarget)[1])
    doesNotMatch <- names(targets) != weightTargetNames
    if(any(doesNotMatch)) warning("w8margin column name(s) ", paste(weightTargetNames[doesNotMatch], collapse = ", "), " do not match list name(s) ",  paste0(names(weightTargetNames)[doesNotMatch], collapse = ", "), "; coercing to match column name")

    names(targets) <- weightTargetNames
  }
  
  
  
  ## ==== PROCESS TARGETS ====
  
  #Check if target exists for weighting variables
  missing_from_observed <- !(weightTargetNames %in% names(design$variables))
  if(sum(missing_from_observed) > 0) stop(paste("Observed data was not found for weighting variables ", toString(weightTargetNames[missing_from_observed], sep = ", ")))
 
  # ---- Change target level names if match.levels.by = "order" ----
  #Changes to target levels, for variables where match.levels.by = "order" (this tells us that the first row of the target should automatically be the first level of the variable, and so on)

  forcedLevels <- mapply(function(target, observed, varname, isForced){
      if(isForced == FALSE) return(NULL)
      else{
          if(length(levels(observed)) == target.length(target)) return(levels(observed)) #If length of target matches levels of observed, return that
          else if(length(levels(factor(observed))) == target.length(target)) return(levels(factor(observed))) #If length doesn't match, see if refactoring to drop empty levels will help
          else stop("Length of target for variable '", varname, "' did not match number of levels in observed data")
      }
  }, target = targets, observed = design$variables[weightTargetNames], varname = weightTargetNames, isForced = match.levels.by == "order", SIMPLIFY = FALSE)

  
  # ---- Convert targets to class w8margin ----
  #First save original samples sizes (for diagostics later) 
  #Feed in temporary names here
  origTargetSums <- mapply(function(targets, forcedLevels, weightTargetNames){
      sum(as.w8margin(target = targets, varname = weightTargetNames, levels = forcedLevels)$Freq)
  }, targets = targets, forcedLevels = forcedLevels, weightTargetNames = weightTargetNames)

  #Then compute actually weight targets
  #Feed in temporary names here, so that we can check the length/number of levels and 
  targets <- mapply(as.w8margin, target = targets, varname = weightTargetNames, levels = forcedLevels,
                          MoreArgs = list(samplesize = samplesize), SIMPLIFY = FALSE)
  
  
  ## ==== HANDLE ZERO WEIGHTS ====
  
  #remove levels with target of "0" from observed data and target ----
  #survey's "rake" command will not accept a target of zero, so we need to manually drop it from a data frame and w8margin object
  design$variables[,weightTargetNames] <- lapply(design$variables[, weightTargetNames, drop = FALSE], as.factor)
  
  # ---- Identify target levels of zero, and remove them from the targets ----
  zeroTargetLevels <- lapply(targets, function(onetarget) as.character(onetarget[!is.na(onetarget$Freq) & onetarget$Freq == 0, 1])) #identify zero levels
  targets <- lapply(targets, function(onetarget) onetarget[is.na(onetarget$Freq) | onetarget$Freq != 0, ]) #drop zero levels from targets
  
  # ---- Remove cases (and factor levels) associated with zero targets from the data, or zero design weights
  #Identify cases that have a zero target on at least one variable, or a design weight of zero
  merge_index.df <- data.frame(index = 1:nrow(design$variables), keepYN = TRUE)
  
  if(any(sapply(zeroTargetLevels, length) > 0) | any(weights(design) == 0)){
      #Identify cases that will be dropped based on empty target levels
      merge_index.df$keepYN <-
         rowSums(
             simplify2array(mapply(
                 function(var, levelsToDrop, varname){
                     if(length(levelsToDrop) > 0){
                         factorLevels <- levels(as.factor(var))
                         isValidLevel <- levelsToDrop %in% factorLevels
                         if(any(!isValidLevel)) warning("Empty target level(s) ", toString(levelsToDrop[!isValidLevel], sep = ", "), " do not match with any observed data on variable ", varname) 
                         var %in% levelsToDrop
                     }
                     else rep(FALSE, length(var))
                }, var = design$variables[weightTargetNames], levelsToDrop = zeroTargetLevels, varname = weightTargetNames)
             )
        ) == 0
      
      #Identify casess that have a design weight of zero
      merge_index.df$keepYN[weights(design) == 0] <- FALSE
      
      #Check which factor levels have valid cases, before dropping cases
      predrop.tab <- lapply(design$variables[weightTargetNames], table)
      
      #drop cases
      design <- subset(design, merge_index.df$keepYN)
      
      #remove the unneeded factor levels
      design$variables[weightTargetNames] <- mapply(function(factorvar, levelsToDrop){
          if(length(levelsToDrop) > 0) factor(factorvar, levels = levels(factorvar)[!(levels(factorvar) %in% levelsToDrop)])
          else factorvar
      }, factorvar = design$variables[weightTargetNames], levelsToDrop = zeroTargetLevels, SIMPLIFY = FALSE)
      
      #Check if we are accidentally losing all cases (on factor levels with valid targets) by dropping some casses
      postdrop.tab <- lapply(design$variables[weightTargetNames], table) #Table after dropping cases
      mapply(function(pre, post, levelsToDrop, varname){   #Compare tables before and after dropping
          pre <- pre[!(names(pre) %in% levelsToDrop)]
          post <- post[!(names(pre) %in% levelsToDrop)]
          
          lostAllCases <- post == 0 & pre != 0
          if(any(lostAllCases)) warning("All valid cases for ", varname, " level(s) ", toString(names(pre)[lostAllCases]), " had weight zero and were dropped")
      }, pre = predrop.tab, post = postdrop.tab, levelsToDrop = zeroTargetLevels, varname = weightTargetNames)
      
  }
  
  
  ## ==== CHECK THAT TARGETS ARE VALID ====
  
  #Check if targets currently match
  isTargetMatch <- mapply(target.matched, w8margin = targets, observedVar = design$variables[, weightTargetNames, drop = FALSE])
  #Check if targets would match after re-factoring (re-factoring might produce less helpful messages)
  suppressWarnings(isRefactoredMatch <- mapply(target.matched, w8margin = targets, observedVar = design$variables[, weightTargetNames, drop = FALSE],
                                               refactor = TRUE))
  #Solve issues that can be solved with refactoring, stop if refactoring can't solve issues
  if(any(!isRefactoredMatch)) stop("Target does not match observed data on variable(s) ", paste(weightTargetNames[!isTargetMatch], collapse = ", "))
  else if(any(!isTargetMatch)) design$variables[,weightTargetNames][!isTargetMatch] <- lapply(design$variables[, weightTargetNames, drop = FALSE][!isTargetMatch], factor)
  
  
  
  ## ==== CHECK FOR CONSISTENT SAMPLE SIZES ====
  
  #to handle objects that were *not* coerced to a set sample size, check that samplesize for each w8margin is the same
  finalTargetSums <- sapply(targets, function(x) sum(x$Freq))
  isSizeTolerated <- (finalTargetSums / samplesize) < (1 + rebase.tol) & (finalTargetSums / samplesize) > (1 - rebase.tol)
  if(any(isSizeTolerated == FALSE)) stop("Target sample sizes vary by more than specified tolerance; try changing rebase.tol")

  #to handle objects that were coerced to a set sample size, check if any of the sample sizes required substantive rebasing
  rebaseRatio <- lapply(origTargetSums, function(origSum) c(1, 100, samplesize) / origSum)
  #Compute the ratio of 1, 100, and the original sample size to OrigSum
  isRebaseTolerated <- sapply(rebaseRatio, function(ratio) any((ratio > (1 - rebase.tol)) & (ratio < (1 + rebase.tol)))) #check if the ratio is 1 +- some tolerancee
  if(any(isRebaseTolerated == FALSE)) warning("targets for variable ", toString(names(targets)[!isRebaseTolerated], sep = ", "), " sum to ", toString(origTargetSums[!isRebaseTolerated], sep = ", "), " and were rebased")
  
  
  
  ## ==== RUN WEIGHTS ====
  
  sample.margins <- lapply((paste0("~", weightTargetNames)), as.formula)
  population.margins <- targets
  
  weighted <- survey::rake(design = design, sample.margins = sample.margins, population.margins = population.margins, ...)
  
  merge_index.df$weight <- 0
  merge_index.df$weight[merge_index.df$keepYN == TRUE] <- weights(weighted)
  
  return(merge_index.df$weight)
}



## ==== MISCELLANEOUS FUNCTIONS ====

#' Kish's approximate weighting efficiency
eff.n <- function(design){
  myweights <- survey::weights(design)
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
