require("survey")

#major to-do list:
# 1) write "weight cleaning" function that reorders, trims whitespace, removes empty levels, etc
# 2) make sure we can handle tables as inputs, and perhaps arrays of >2 dimensions
# 3) test more

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
# the Freq colun coontains the *count* (not percent) of each level, as desired by "rake"

#TO DO:
#Might want to add a warning about trailing whitespace?
#Add a default value for samplesize, taken from observed

as.w8target.matrix <- function(target, samplesize, varname, varlevels = NULL, byrow = TRUE){
  require(gdata) #for the "unmatrix" function
  target.matrix <- target
  
  ## ---- set names for target levels ----
  target.vector <- unmatrix(target.matrix, byrow = byrow)
  if(is.null(varlevels)){  #set w8target levels based on row and column names, if "varlevels" is not specified
    if(sum(is.na(rownames(target.matrix))) > 0 | is.null(rownames(target.matrix))) stop("Matrix has invalid or missing row names")
    if(sum(is.na(colnames(target.matrix))) > 0 | is.null(colnames(target.matrix))) stop("Matrix has invalid or missing column names")
    names(target.vector) <- gsub(":", ".", names(target.vector))
  } else{  #set w8target levels based on varlevels input, if ut us specified
    if(length(varlevels) != length(target.vector)) stop("varlevels must be of length", length(target.vector))
    names(target.vector) <- varlevels
  }
  
  duplicates <- duplicated(names(target.vector))
  if(sum(duplicates) > 0) stop("target level name ", names(target.vector[duplicates]), " is duplicated in target file")
  
  ## ---- rebase targets to sample size ----
  origSum <- sum(target.vector)
  if(origSum != 1 & origSum != 100){
    warning("targets for variable ", varname, " sum to ", origSum, " and will be rebased")
  }
  target.counts <- (target.vector / origSum) * samplesize 
  
  ## ---- generate output object ----
  w8target <- data.frame(names(target.counts), target.counts)
  names(w8target) <- c(varname, "Freq")
  
  class(w8target) <- c("w8target", "data.frame")
  return(w8target)
}

as.w8target.data.frame <- function(target, samplesize, varname = NULL, varlevels = NULL){
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
  if(origSum != 1 & origSum != 100){
    warning("targets for variable ", varname, " sum to ", origSum, " and will be rebased")
  }
  w8target$Freq <- (target.df$Freq / origSum) * samplesize #rebase targets to sample size
  
  ## ---- generate output object ----
  if(!is.null(varlevels)) w8target[names(w8target) != "Freq"] <- varlevels
  if(!is.null(varname)) names(w8target)[names(w8target) != "Freq"] <- varname
  
  class(w8target) <- c("w8target", "data.frame")
  return(w8target)
}

as.w8target.numeric <- function(target, samplesize, varname, varlevels = NULL){
  target.numeric <- target
  
  ## ---- error handling ----
  if(is.null(varlevels)){
    if(sum(is.na(names(target.numeric))) > 0 | is.null(names(target.numeric))) stop("Vector has invalid or missing names - try specifying varlevels")
  } else{
    if(length(varlevels) != length(target.numeric)) stop("varlevels must be of length", length(target.numeric))
    names(target.numeric) <- varlevels
  }
  duplicates <- duplicated(names(target.numeric))
  if(sum(duplicates) > 0) stop("target level name ", names(target.numeric[duplicates]), " is duplicated in target file")
  
  
  ## ---- rebase targets to sample size ----
  origSum <- sum(target.numeric)
  if(origSum != 1 & origSum != 100){
    warning("targets for variable ", varname, " sum to ", origSum, " and will be rebased")
  }
  target.counts <- (target.numeric / origSum) * samplesize #rebase targets to sample size
  
  ## ---- generate output object ----
  w8target <- data.frame(names(target.numeric), target.numeric)
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

#Input: "w8target" - a w8target object containg target data; "data" - a column of observed factor data
#Output: a boolean, whether we think target_list and observed_data will be compatible; along with a warning message explainig the failure if FALSE is returned

#TO DO:
#Use rounding when checking if numbers sum to 100%
#Consider flagging trailing whitespace in target_list or levels(observed_data)
#Extract name from w8target and use in warning messages
#accept svydesign rather than data object, and check whether *frequency-weighted* data contains all needed variables

checkTargetMatch <- function(w8target, data){
  
  ## --- Error handling ----
  if(is.factor(data) == FALSE){
    warning("observed data is not a factor variable")
    return(FALSE)
  }
  
  if(!("w8target" %in% class(w8target))){
    warning("target is not a w8target object")
    return(FALSE)
  }
  obs_levels <- levels(data)
  
  ## ---- Check for empty levels in data and target ----
  emptyData <- summary(data) == 0
  hasEmptyData <- sum(emptyData)
  emptyTarget <- w8target$Freq == 0
  hasEmptyTarget <- sum(emptyTarget)
  
  if(hasEmptyData > 0 | hasEmptyTarget > 0){
    if(hasEmptyData > 0) warning("observed data contains empty factor level ", levels(data)[emptyData])
    if(hasEmptyTarget > 0)  warning("weight target contains empty factor level ", obs_levels[emptyTarget])
    return(FALSE)
  }
  
  ## ---- Check if number of levels in observed data matches length of target ----
  if(length(w8target[,1]) != length(obs_levels)){
    warning("number of variable levels in observed data does not match length of target")
    return(FALSE)
  }
  
  ## ---- Check for levels in observed data that do not match levels in target ----
  if(sum(w8target[,1] != obs_levels) > 0){
    #Check if sorted variable levels are the same
    if(sum(sort(w8target[,1]) != sort(obs_levels)) == 0){
      warning("variable levels in observed data are sorted differently from target")
      return(FALSE)
    }
    
    #Identify missing levels in both observed and target data
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

#I want to make substantial changes to this function, so that it coerces other weight target formats to a w8target object

#Input: "design", an svydesign object or else a data.frame that can be coerced to an svydesign object
# "weightTargets", a list of w8target objects (I want to change this so it takes a more flexible format)
# "weightTarget.id", a character  string that specificies whether we get the names of weight target variables from the named items of a list, or the columns of data frames within the list
# sampleSize - an integer with the desired post-weight sample size
#Output: a weighhted svydesign object

#TO DO
#Add samplesize = "original" option, to take sample sizes from observed values (and check to make sure they're the same for all w8target objects)
#Don't rename columns of data frames when converting to w8target

#Consider adding a a "force" parameter to change the targets to match with observed data, based on "order" (force first observed level to match with first target level) or "name" (re-sort according to name)
#Think about ways to automatically handle minor problems with checkTargetMatch - trailing whitespace, differently sorted variable levels

quickRake <- function(design, weightTargets, samplesize = "observed", weightTarget.id = "listname", ...){
  require(survey)
  
  ## ---- Convert objects to needed classes ----
  # Convert data frame to svydesign object
  if("data.frame" %in% class(design)){
    design <- svydesign(~0, data = design, control = list(verbose = FALSE))
  } 
  if(samplesize == "observed"){ #"observed" means we want to take a centrally-specified sample size
    samplesize <- sum(weights(design))
  }

  #Convert targets to class w8target
  which.w8target <- sapply(weightTargets, function(x) "w8target" %in% class(x))
  if(sum(!(which.w8target)) > 0){
    warning("targets for variables ", paste(names(weightTargets)[!which.w8target], collapse = ", "), " are not objects of class w8target and will be coerced")
    weightTargets[!which.w8target] <- mapply(as.w8target,
                                             target = weightTargets[!which.w8target], varname = names(weightTargets)[!which.w8target],
                                             samplesize = samplesize,
                                             SIMPLIFY = FALSE)
  }
  
  ## ---- Get names of weighting variables ----
  #Names of weighting variables can be contained in one of two places:
  # A) name of item in the weightTarget list (preferable), applicable even if we use as.w8target to convert target types
  # B) the name of the second column of a w8target object, applicable only if we  do not need to convert targets  to w8target class

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
    weight_target_names <- sapply(weightTargets, function(onetarget) names(onetarget)[1])
    doesNotMatch <- names(weightTargets) != weight_target_names
    warning("w8target column name(s) ", paste(weight_target_names[doesNotMatch], collapse = ", "), " do not match list name(s) ",  paste0(names(weightTarget.id)[doesNotMatch], collapse = ", "), " ; coercing to match column name")
    
    names(weightTargets) <- weight_target_names
  }else stop("invalid specification for weightTarget.id")
  
  
  ## ---- Check that targets and observed data are valid ----
  isTargetMatch <- mapply(checkTargetMatch, w8target = weightTargets, data = design$variables[,weight_target_names])
  if(sum(!isTargetMatch) > 0) stop("Target does not match variable(s) on ", paste(weight_target_names[!isTargetMatch], collapse = ", "))
  
  #if some objects were *not* coerced, check that samplesize for each w8target is the same
  if(sum(which.w8target) != 0){
    samplesize.w8target <- sapply(weightTargets, function(x) sum(x$Freq))
    if(sum(samplesize.w8target[1] != samplesize.w8target[-1]) > 0) stop("Target sample sizes are inconsistent across variables: ", paste(paste(names(weightTarget), samplesize.w8target, sep = "= ")), collapse = "; ")
  }
  
  #Check if target exists for weighting variables
  missing_from_observed <- !(weight_target_names %in% names(design$variables))
  if(sum(missing_from_observed) > 0) stop(paste("Observed data was not found for weighting variables", toString(weight_target_names[missing_from_observed], sep = ", ")))
  
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


