require("survey")

#major to-do list:
# 1) update quickrake to handle common errors
# 1a) targets - drop empty levels, reorder levels to match observed, maybe fix whitespace
# 1b) observed data - forced factor vairable, drop empty levels, maybe  fix whitespace
# 2) make sure we can handle tables as inputs, and perhaps arrays of >2 dimensions
# 3) test more
# 4) consider copying similar function from rake to postStratify
# 5) handle NA targets

## ==== FUNCTIONS TO LOAD TARGETS FROM CSVs ====

# "these are convenvience functions that read in targets from CSVs (how I usually store them, because it's easy)
# however, they need changes

#TO DO:
#delete or greatly change these functions
#because as.w8target requires a sample size, we should generally not convert to w8target objects until calling the rake command

get_matrix_targets <- function(filepath, samplesize, varname = gsub(pattern = ".csv", replacement = "", x = filepath), encoding = "UTF-8"){
  
  target.matrix <- as.matrix(read.csv(filepath, row.names = 1, encoding = encoding))
  w8target <- as.w8target.matrix(target.matrix = target.matrix, samplesize = samplesize, varname = varname)
  
  return(w8target)
}

get_vector_targets <- function(filepath, samplesize, varname = gsub(pattern = ".csv", replacement = "", x = filepath), encoding = "UTF-8"){
  target.df <- read.csv(filepath, header = FALSE, col.names = c(varname, "Freq"), encoding = encoding)
  
  w8target <- as.w8target.data.frame(target.df = target.df, samplesize = samplesize, varname = varname)
  
  return(w8target)
}



## ==== FUNCTIONS TO CONVERT MATRICES, DFS, AND VECTORS TO W8TARGETS ====

# "w8target" is a class I have created to specify the format required for "rake" weighting variables
# it is a data frame with varname and "Freq" columns, and named rows that list each level of the variable
# the Freq colun contains the *count* (not percent) of each level, as desired by "rake"

#TO DO: 
#Consider dropping zero levels and dropping whitespace
#create shared checkTolerance function, called by all methods
#think about as.w8target.w8target and as.w8target.array

as.w8target.matrix <- function(target, varname, samplesize = NULL, forcedLevels = NULL, byrow = TRUE, rebaseTolerance = .01){
  require(gdata) #for the "unmatrix" function
  target.matrix <- target
  
  ## ---- set names for target levels ----
  target.vector <- unmatrix(target.matrix, byrow = byrow)
  if(is.null(forcedLevels)){  #set w8target levels based on row and column names, if "forcedLevels" is not specified
    if(sum(is.na(rownames(target.matrix))) > 0 | is.null(rownames(target.matrix))) stop("Matrix has invalid or missing row names")
    if(sum(is.na(colnames(target.matrix))) > 0 | is.null(colnames(target.matrix))) stop("Matrix has invalid or missing column names")
    names(target.vector) <- gsub(":", ".", names(target.vector))
  } else{  #set w8target levels based on forcedLevels input, if it us specified
    if(length(forcedLevels) != length(target.vector)) stop("forcedLevels must be of length", length(target.vector))
    names(target.vector) <- forcedLevels
  }
  
  duplicates <- duplicated(names(target.vector))
  if(sum(duplicates) > 0) stop("target level name ", names(target.vector[duplicates]), " is duplicated in target file")
  
  ## ---- rebase targets to sample size ----
  origSum <- sum(target.vector)
  if(is.null(samplesize)){
    samplesize <- origSum
  }else{ #generate a warning message if the original target doesn't sum to 1, 100, or samplesize (+- some tolerance)
    checkTolerance.vec <- c(1, 100, samplesize) / origSum #Compute the ratio of 1, 100, and the original sample size to OrigSum
    isTolerated <- sum(checkTolerance.vec > (1 - rebaseTolerance) & checkTolerance.vec < (1 + rebaseTolerance)) #check if the ratio is 1 +- some tolerancee
    if(isTolerated == FALSE) warning("targets for variable ", varname, " sum to ", origSum, " and will be substantively rebased")
  }
  target.counts <- (target.vector / origSum) * samplesize 
  
  ## ---- generate output object ----
  w8target <- data.frame(names(target.counts), target.counts)
  names(w8target) <- c(varname, "Freq")
  
  class(w8target) <- c("w8target", "data.frame")
  return(w8target)
}

as.w8target.data.frame <- function(target, varname = NULL, samplesize = NULL, forcedLevels = NULL, rebaseTolerance = .01){
  target.df <- target
  
  ## ---- error handling ----
  if(!("Freq" %in% names(target.df))) stop("data frames must have Freq column for conversion to w8target")
  if(ncol(target.df) > 2) stop("data frames must have two columns for converstion to w8target")
  
  target_levels <- target.df[,names(target.df) != "Freq"]
  duplicates <- duplicated(target_levels)
  if(sum(duplicates) > 0) stop("target level name ", target_levels[duplicates], " is duplicated in target file")

  ## ---- rebase targets to sample size ----
  w8target <- target.df
  origSum <- sum(target.df$Freq)
  if(is.null(samplesize)){
    samplesize <- origSum
  }else{ #generate a warning message if the original target doesn't sum to 1, 100, or samplesize (+- some tolerance)
    checkTolerance.vec <- c(1, 100, samplesize) / origSum #Compute the ratio of 1, 100, and the original sample size to OrigSum
    isTolerated <- sum(checkTolerance.vec > (1 - rebaseTolerance) & checkTolerance.vec < (1 + rebaseTolerance)) #check if the ratio is 1 +- some tolerancee
    if(isTolerated == FALSE) warning("targets for variable ", varname, " sum to ", origSum, " and will be substantively rebased")
  }
  w8target$Freq <- (target.df$Freq / origSum) * samplesize #rebase targets to sample size
  
  ## ---- generate output object ----
  if(!is.null(forcedLevels)) w8target[names(w8target) != "Freq"] <- forcedLevels
  if(!is.null(varname)) names(w8target)[names(w8target) != "Freq"] <- varname
  
  class(w8target) <- c("w8target", "data.frame")
  return(w8target)
}

as.w8target.numeric <- function(target, varname, samplesize = NULL, forcedLevels = NULL, rebaseTolerance = .01){
  target.numeric <- target
  
  ## ---- error handling ----
  if(is.null(forcedLevels)){
    if(sum(is.na(names(target.numeric))) > 0 | is.null(names(target.numeric))) stop("Vector has invalid or missing names - try specifying forcedLevels")
  } else{
    if(length(forcedLevels) != length(target.numeric)) stop("forcedLevels must be of length", length(target.numeric))
    names(target.numeric) <- forcedLevels
  }
  duplicates <- duplicated(names(target.numeric))
  if(sum(duplicates) > 0) stop("target level name ", names(target.numeric[duplicates]), " is duplicated in target file")
  
  
  ## ---- rebase targets to sample size ----
  origSum <- sum(target.numeric)
  if(is.null(samplesize)){
    samplesize <- origSum
  }else{ #generate a warning message if the original target doesn't sum to 1, 100, or samplesize (+- some tolerance)
    checkTolerance.vec <- c(1, 100, samplesize) / origSum #Compute the ratio of 1, 100, and the original sample size to OrigSum
    isTolerated <- sum(checkTolerance.vec > (1 - rebaseTolerance) & checkTolerance.vec < (1 + rebaseTolerance)) #check if the ratio is 1 +- some tolerancee
    if(isTolerated == FALSE) warning("targets for variable ", varname, " sum to ", origSum, " and will be substantively rebased")
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

#checkTargetMatch checks whether w8targets match with observed data
#This is one of the most common reasons why rake fails in my experience

#Input: "w8target" - a w8target object containg target data; 
  #"data" - a column of observed factor data
  #"exact" - a boolean, specifying whether the target data must come in the same order as the observed factor levels
#Output: a boolean, whether we think target_list and observed_data will be compatible; along with a warning message explainig the failure if FALSE is returned

#TO DO:
#Consider flagging trailing whitespace in target_list or levels(observed_data)
#Extract name from w8target and use in warning messages
#accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables
#check for missing data in observeds

checkTargetMatch <- function(w8target, observedVar, exact = FALSE){
  
  ## --- Error handling ----
  if(is.factor(observedVar) == FALSE){
    warning("observed data is not a factor variable")
    return(FALSE)
  }
  
  if(!("w8target" %in% class(w8target))){
    warning("target is not a w8target object and will be coerced")
    w8target <- as.w8target(w8target, samplesize = 1, varname = "var")
  }
  obs_levels <- levels(observedVar)
  
  ## ---- Check for empty levels in observedVar and target ----
  emptyObserved <- summary(observedVar) == 0
  hasEmptyObserved <- sum(emptyObserved)
  emptyTarget <- w8target$Freq == 0
  hasEmptyTarget <- sum(emptyTarget)
  
  if(hasEmptyObserved > 0 | hasEmptyTarget > 0){
    if(hasEmptyObserved > 0) warning("observed data contains empty factor level ", levels(observedVar)[emptyObserved])
    if(hasEmptyTarget > 0)  warning("weight target contains empty factor level ", obs_levels[emptyTarget])
    return(FALSE)
  }
  
  ## ---- Check if number of levels in observed data matches length of target ----
  if(length(w8target[,1]) != length(obs_levels)){
    warning("number of variable levels in observed data does not match length of target")
    return(FALSE)
  }
  
  ## ---- Check for levels in observed data that do not match levels in target ----
  
  #If exact == TRUE, check if unsorted levels are the same in target and observed
  if(exact == TRUE & (sum(w8target[,1] != obs_levels) > 0) & (sum(sort(w8target[,1]) != sort(obs_levels)) == 0)){
    warning("variable levels in target are in differet order from observed factor variable")
    return(FALSE)
  }
  #otherwise, check if *sorted* variable levels are the same
  if(sum(sort(w8target[,1]) != sort(obs_levels)) > 0){
    #Identify missing levels in both observed and target 
    missing_from_target.index <- !(w8target[,1] %in% obs_levels)
    missing_from_obs.index <- !(obs_levels %in% w8target[,1])
    missing_from_target.string <- paste(w8target[missing_from_target.index, 1], collapse = ", ")
    missing_from_obs.string <- paste(obs_levels[missing_from_obs.index], collapse = ", ")
    
    if(sum(missing_from_target.index) > 0) warning("variable levels ", missing_from_target.string, " in target are missing from observed factor variable")
    if(sum(missing_from_obs.index) > 0) warning("variable levels ", missing_from_obs.string, " in observed factor variable are missing from target")
    
    return(FALSE)
  }
    
  #If all checks pass, return TRUE
  return(TRUE)
}



## ==== QUICKRAKE ====

#This is the workhorse function - a wrapper for "rake" that is intended to take targets in a more flexible format
#However, flexibility also can be dangerous!

#Input: "design", an svydesign object or else a data.frame that can be coerced to an svydesign object
# "weightTargets", a list of w8target objects (I want to change this so it takes a more flexible format)
# "weightTarget.id", a character  string that specificies whether we get the names of weight target variables from the named items of a list, or the columns of data frames within the list
  #"colname" - get target names from the first column of a w8target object
  #"listname" - get target names from a named list
# sampleSize - either an integer with the desired post-weight sample size, or character string "observed" specifyting that the observed sample size is correct)
# MatchTargetsBy "levels", "none", or "order" - a variable that specifies how to match levels in the target with the observed data
  # "name" (default) matches based on name, disregarding order (so the "male" target will be matched with the "male" observed data)
  # "order" matches based on order, disregarding name (so the first element in the target will match with the first level of the observed factor variable )
  # "exact" (not y4et implemented) requires that target and observed have the exact same names, and the exact same order


#Output: a weighted svydesign object

#TO DO
#Coerce data columns to factor type and drop empty levels
#Add samplesize = "original" option, to take sample sizes from observed values (and check to make sure they're the same for all w8target objects)
#Don't rename columns of data frames when converting to w8target
#allow weightarget.id to be specified sepaarately for each weighting variable


quickRake <- function(design, weightTargets, samplesize = "observed", matchTargetsBy = "name", weightTarget.id = "listname", ...){
  require(survey)
  
  ## ---- Check for valid valuese on inputs ----
  if(sum(!(matchTargetsBy %in% c("name", "order", "exact"))) > 0) stop("Invalid value(s)", paste(matchTargetsBy[!(matchTargetsBy %in% c("name", "order", "exact"))])," in matchTargetsBy")
  if(sum(!(weightTarget.id %in% c("colname", "listname"))) > 0) stop("Invalid value(s)", paste(weightTarget.id[!(weightTarget.id %in% c("colname", "listname"))])," in weightTarget.id")
  
  
  ## ---- Convert misc objects to needed classes ----
  # Convert data frame to svydesign object
  if("data.frame" %in% class(design)){
    design <- svydesign(~0, data = design, control = list(verbose = FALSE))
  } 
  if(samplesize == "observed"){ #"observed" means we want to take a centrally-specified sample size
    samplesize <- sum(weights(design))
  }
  
  ## ---- Get names of weighting variables ----
  #Names of weighting variables can be contained in one of two places:
  # A) name of item in the weightTarget list (preferable), applicable even if we use as.w8target to convert target types
  # B) the name of the second column of a w8target object, applicable only if we  do not need to convert targets  to w8target class
  which.w8target <- sapply(weightTargets, function(x) "w8target" %in% class(x))
  
    if(weightTarget.id == "listname"){
    weight_target_names <- names(weightTargets) #set weight_target_names  convenience variables to equal the list names
    weightTargets[which.w8target] <- mapply(function(w8target, varname){ #for any targets that were originally in w8target format: change column name to match list name, and generate a warning
      if(colnames(w8target)[1] != varname){
        warning("w8target column name ", colnames(w8target)[1], " does not match list name ",  varname, " ; coercing to match list name")
        colnames(w8target)[1] <- varname
      }
      return(w8target)
    }, w8target = weightTargets[which.w8target], varname = weight_target_names[which.w8target], SIMPLIFY = FALSE)
  }else if(weightTarget.id == "colname"){
    #SHOULD PROBABLY CHECK THAT ALL OBJECTS ARE DATA FRAMES OR W8TARGETS IF WEIGHTTARGETID = COLNAME
    weight_target_names <- sapply(weightTargets, function(onetarget) names(onetarget)[1])
    doesNotMatch <- names(weightTargets) != weight_target_names
    warning("w8target column name(s) ", paste(weight_target_names[doesNotMatch], collapse = ", "), " do not match list name(s) ",  paste0(names(weightTarget.id)[doesNotMatch], collapse = ", "), " ; coercing to match column name")
    
    names(weightTargets) <- weight_target_names
  }else stop("invalid specification for weightTarget.id")
  
  ## ---- Change targets ----
  #Check if target exists for weighting variables
  missing_from_observed <- !(weight_target_names %in% names(design$variables))
  if(sum(missing_from_observed) > 0) stop(paste("Observed data was not found for weighting variables", toString(weight_target_names[missing_from_observed], sep = ", ")))
  
  ## ---- Convert targets to class w8target ----
  
  # Force names of targets to follow observed factor names OR sort by observed order
  # if matchTargetsBy is a scalar, repeat it for every variable
  if(length(matchTargetsBy) == 1) matchTargetsBy <- rep(matchTargetsBy, length(weightTargets))
  
  #NEED TO ADD HANDLING FOR ONLY ONE WEIGHT VARIABLE
  forcedTargetLevels <- mapply(function(var, forceType){
    if(forceType == "order"){ forcedTargetLevels <- levels(var)} else forcedTargetLevels <- NULL
  }, var = design$variables[,weight_target_names], forceType = matchTargetsBy)
  
  ## ---- Convert targets to class w8target ----
  
  if(sum(!(which.w8target)) > 0) warning("targets for variables ", paste(names(weightTargets)[!which.w8target], collapse = ", "), " are not objects of class w8target and will be coerced")
  
  #HOW DO I HANDLE W8TARGET OBJECTS THAT MAY STILL NEED CHANGING? do I need an as.w8target.w8target method?
  weightTargets <- mapply(as.w8target,
                                             target = weightTargets, varname = names(weightTargets), forcedLevels = forcedTargetLevels,
                                             MoreArgs = list(samplesize = samplesize),
                                             SIMPLIFY = FALSE)
  
  
  
  ## ---- Check that targets and observed data are valid ----
  isTargetMatch <- mapply(checkTargetMatch, w8target = weightTargets, observedVar = design$variables[,weight_target_names],
                          exact = (matchTargetsBy == "exact"))
  if(sum(!isTargetMatch) > 0) stop("Target does not match variable(s) on ", paste(weight_target_names[!isTargetMatch], collapse = ", "))
  
  #if some objects were *not* coerced, check that samplesize for each w8target is the same
  samplesize.w8target <- sapply(weightTargets, function(x) sum(x$Freq))
  #BROKEN - NEED TO ACCOUNT FOR TOLERANCE
  #if(sum(samplesize.w8target[1] != samplesize.w8target[-1]) > 0) stop("Target sample sizes are inconsistent across variables: ", paste(paste(names(weightTarget), samplesize.w8target, sep = "= ")), collapse = "; ")

  
  
  ## ---- Run weights ----
  sample.margins <- lapply((paste0("~", weight_target_names)), as.formula)
  population.margins <- weightTargets
  
  weighted <- rake(design = design, sample.margins = sample.margins, population.margins = population.margins, ...)
  return(weighted)
}



#===MISCELLANEOUS FUNCTIONS===

#Kish's approximate weighting efficiency
eff_n <- function(design){
  myweights <- weights(design)
  eff_n <- (sum(myweights) ^ 2) / (sum(myweights ^ 2))
  return(eff_n)
}


