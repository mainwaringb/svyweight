require("survey")

#major to-do list:
#quickrake
    #1) Don't rename columns of data frames when converting to w8target

#nice to have
#as.w8target
    #1) think about as.w8target.w8target and as.w8target.array
    #2) create shared "checkTolerance" function
#checktargetmatch:
    #1) accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables
## quickrake:
    # 1)allow weightarget.id and refactor to be specified separately for each weighting variable



## ==== FUNCTIONS TO CONVERT MATRICES, DFS, AND VECTORS TO W8TARGETS ====

# "w8target" is a class I have created to specify the format required for "rake" weighting variables
# it is a data frame with varname and "Freq" columns, and named rows that list each level of the variable
# the Freq colun contains the *count* (not percent) of each level, as desired by "rake"

#TO DO: 
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

#checkTargetMatch checks whether w8targets match with observed data
#This is one of the most common reasons why rake fails in my experience

#Input: "w8target" - a w8target object containg target data; 
  #"data" - a column of observed factor data
  #"exact" - a boolean, specifying whether the target data must come in the same order as the observed factor levels
  #"refactor" - a boolean, specifying whether a variable should be (re)factored before checking match
#Output: a boolean, whether we think target_list and observed_data will be compatible; along with a warning message explainig the failure if FALSE is returned

#TO DO:
#accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables

checkTargetMatch <- function(w8target, observedVar, exact = FALSE, refactor = FALSE){
  
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
  
  #If exact == TRUE, check if unsorted levels are the same in target and observed
  if(exact == TRUE & (sum(w8target[,1] != obs_levels) > 0) & (sum(sort(w8target[,1]) != sort(obs_levels)) == 0)){
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



## ==== QUICKRAKE ====

#This is the workhorse function - a wrapper for "rake" that is intended to take targets in a more flexible format
#However, flexibility also can be dangerous!

#Input: "design", an svydesign object or else a data.frame that can be coerced to an svydesign object
# "weightTargets", a list of w8target objects 
# "weightTarget.id", a character  string that specificies whether we get the names of weight target variables from the named items of a list, or the columns of data frames within the list
  #"colname" - get target names from the first column of a w8target object
  #"listname" - get target names from a named list
# sampleSize - either an integer with the desired post-weight sample size, or character string "fromData" specifyting that the observed sample size is correct)
# MatchTargetsBy "name", "exact", or "order" - a variable that specifies how to match levels in the target with the observed data
  # "name" (default) matches based on name, disregarding order (so the "male" target will be matched with the "male" observed data)
  # "order" matches based on order, disregarding name (so the first element in the target will match with the first level of the observed factor variable )
  # "exact" (not yet implemented) requires that target and observed have the exact same names, and the exact same order
# refactor - should variables be (re)factored before attmpting to weight?

#Output: a weighted svydesign object

#TO DO
#Don't rename columns of data frames when converting to w8target
#allow weightarget.id and refactor to be specified separately for each weighting variable

quickRake <- function(design, weightTargets, samplesize = "fromData", matchTargetsBy = "name", weightTarget.id = "listname", rebaseTolerance = .01, refactor = TRUE, ...){
  require(survey)
  
    
  ## ==== HOUSEKEEPING ====
  # ---- Check for valid values on inputs ----
  if(sum(!(matchTargetsBy %in% c("name", "order", "exact"))) > 0) stop("Invalid value(s) ", paste(matchTargetsBy[!(matchTargetsBy %in% c("name", "order", "exact"))])," in matchTargetsBy")
  if(sum(!(weightTarget.id %in% c("colname", "listname"))) > 0) stop("Invalid value(s) ", paste(weightTarget.id[!(weightTarget.id %in% c("colname", "listname"))])," in weightTarget.id")
  
  
  # ---- Convert misc objects to needed classes ----
  # Convert data frame to svydesign object
  if("data.frame" %in% class(design)){
      #Notice that we are suppressing the warning here - svydesign will otherwise produce a warning that no input weights are provided
      suppressWarnings(design <- svydesign(~0, data = design, control = list(verbose = FALSE)))
  } 
    
  # Define sample size 
  if(samplesize == "fromData"){ #"fromData" means we want to take a centrally-specified sample size
    samplesize <- sum(weights(design))
  } else if(samplesize == "fromTargets"){ #"fromTargets" means we want to take the sample size found in the targets, IE not specify one here
      samplesize <- NULL
  }
    
  #If a single vector/dataframe/matrix/weighttarget is ps
  if(!("list" %in% class(weightTargets))) weightTargets <- list(weightTargets)
    
  ## ==== IDENTIFY NAMES OF WEIGHTING VARIABLES ====
    
  #Names of weighting variables can be contained in one of two places:
  # A) name of item in the weightTarget list (preferable), applicable even if we use as.w8target to convert target types
  # B) the name of the second column of a w8target object, applicable only if targets are class w8target or data frame
  which.w8target <- sapply(weightTargets, function(x) "w8target" %in% class(x))
  
  if(weightTarget.id == "listname"){
    weight_target_names <- names(weightTargets) #set weight_target_names  convenience variables to equal the list names
    if(length(unique(weight_target_names)) < length(weightTargets)){
      if(is.null(weight_target_names)) stop("List of weight targets must be named unless weightTarget.id is set to 'colnames'")
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
    
  }else if(weightTarget.id == "colname"){
    if(any(!which.w8target)) stop("weightTarget.id = 'colname' requires targets of class w8target")
      
    weight_target_names <- sapply(weightTargets, function(onetarget) names(onetarget)[1])
    doesNotMatch <- names(weightTargets) != weight_target_names
    if(any(doesNotMatch)) warning("w8target column name(s) ", paste(weight_target_names[doesNotMatch], collapse = ", "), " do not match list name(s) ",  paste0(names(weight_target_names)[doesNotMatch], collapse = ", "), "; coercing to match column name")

    names(weightTargets) <- weight_target_names
  }
  
  ## ==== PROCESS TARGETS ====
  
  #Check if target exists for weighting variables
  missing_from_observed <- !(weight_target_names %in% names(design$variables))
  if(sum(missing_from_observed) > 0) stop(paste("Observed data was not found for weighting variables ", toString(weight_target_names[missing_from_observed], sep = ", ")))
  
  # ---- identify target levels that should be changed to follow observed variable levels ----

  # if matchTargetsBy is a scalar, repeat it for every variable
  if(length(matchTargetsBy) == 1) matchTargetsBy <- rep(matchTargetsBy, length(weightTargets))
  else if(length(matchTargetsBy) != length(weightTargets)) stop("incorrect length for matchTargetsBy")
  
  #(Re)factor variables in observed data (we do this now, so that we can easily get the levels of the observed variable and match the targets to them)
  if(refactor == TRUE){
      design$variables[,weight_target_names] <- lapply(design$variables[, weight_target_names, drop = FALSE], factor)
  }
 
  #Change levels of target, for variables where matchTargetBy = "order" (this tells us that the first row of the target should automatically be the first level of the variable, and so on)
  #NEED TO ADD HANDLING FOR ONLY ONE WEIGHT VARIABLE
  forcedTargetLevels <- mapply(function(var, forceType){
    if(forceType == "order"){ forcedTargetLevels <- levels(var)} else forcedTargetLevels <- NULL
  }, var = design$variables[,weight_target_names, drop = FALSE], forceType = matchTargetsBy)
  
  # ---- Convert targets to class w8target ----
  #HOW DO I HANDLE W8TARGET OBJECTS THAT MAY STILL NEED CHANGING? do I need an as.w8target.w8target method?
  #First save original samples sizes (for diagostics later) 
  origTargetSums <- lapply(weightTargets, function(x) sum(as.w8target(x, varname = "x")$Freq))
  
  #Then compute actually weight targets
  weightTargets <- mapply(as.w8target,
                                             target = weightTargets, varname = names(weightTargets), forcedLevels = forcedTargetLevels,
                                             MoreArgs = list(samplesize = samplesize),
                                             SIMPLIFY = FALSE)
  
  
  ## ---- remove levels with target of "0" from observed data and target ----
  # survey's "rake" command will not accept a target of zero, so we need to manually drop it from a data frame and w8target object
  
  # Identify target levels of zero, and remove them from the w8target objects
  zeroTargetLevels <- lapply(weightTargets, function(onetarget) as.character(onetarget[!is.na(onetarget$Freq) & onetarget$Freq == 0, 1])) #identify zero levels
  weightTargets <- lapply(weightTargets, function(onetarget) onetarget[is.na(onetarget$Freq) | onetarget$Freq != 0, ]) #drop zero levels from targets
  
  #Remove these cases and factor levels from data
  dropIndex <- simplify2array(mapply(function(factorvar, levelsToDrop){
      if(length(levelsToDrop) > 0) factorvar %in% levelsToDrop
      else rep(FALSE, length(factorvar))
    }, factorvar = design$variables[names(weightTargets)], levelsToDrop = zeroTargetLevels))
  #drop them
  design <- subset(design, rowSums(dropIndex) == 0)
  #remove the unneeded factor levels
  design$variables[names(weightTargets)] <- mapply(function(factorvar, levelsToDrop){
      if(length(levelsToDrop) > 0) factor(factorvar, levels = levels(factorvar)[!(levels(factorvar) %in% levelsToDrop)])
      else factorvar
  }, factorvar = design$variables[names(weightTargets)], levelsToDrop = zeroTargetLevels, SIMPLIFY = FALSE)
  
  
  ## ==== CHECK THAT TARGETS ARE VALID ====
  
  isTargetMatch <- mapply(checkTargetMatch, w8target = weightTargets, observedVar = design$variables[, weight_target_names, drop = FALSE],
                          exact = (matchTargetsBy == "exact"))
  if(sum(!isTargetMatch) > 0) stop("Target does not match observed data on variable(s) ", paste(weight_target_names[!isTargetMatch], collapse = ", "))
  
  ## ==== CHECK FOR CONSISTENT SAMPLE SIZES ====
  
  #to handle objects that were *not* coerced to a set sample size, check that samplesize for each w8target is the same
  finalTargetSums <- sapply(weightTargets, function(x) sum(x$Freq))
  isSizeTolerated <- (finalTargetSums / samplesize) < (1 + rebaseTolerance) & (finalTargetSums / samplesize) > (1 - rebaseTolerance)
  if(any(isSizeTolerated == FALSE)) stop("Target sample sizes vary by more than specified tolerance; try changing rebaseTolerance")

  #to handle objects that were coerced to a set sample size, check if any of the sample sizes required substantive rebasing
  rebaseRatio <- lapply(origTargetSums, function(origSum) c(1, 100, samplesize) / origSum)
  #Compute the ratio of 1, 100, and the original sample size to OrigSum
  isRebaseTolerated <- sapply(rebaseRatio, function(ratio) any((ratio > (1 - rebaseTolerance)) & (ratio < (1 + rebaseTolerance)))) #check if the ratio is 1 +- some tolerancee
  if(any(isRebaseTolerated == FALSE)) warning("targets for variable ", toString(name(weightTargets)[!isRebaseTolerated], sep = ", "), " sum to ", toString(origTargetSums[!isRebaseTolerated], sep = ", "), " and were rebased")
  
  ## ==== RUN WEIGHTS ====
  
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


