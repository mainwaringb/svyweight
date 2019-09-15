require("survey")



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
  
  w8target <- as.w8target.df(target.df = target.df, samplesize = samplesize, varname = varname)
  
  return(w8target)
}



## ==== FUNCTIONS TO CONVERT MATRICES, DFS, AND VECTORS TO W8TARGETS ====

# "w8target" is a class I have created to specify the format required for "rake" weighting variables
# it is a data frame with varname and "Freq" columns, and named rows that list each level of the variable
# the Freq colun coontains the *count* (not percent) of each level, as desired by "rake"

#TO DO:
#All as.w8target functions should check for duplicate row names, as this will cause  an error in "rake"
#Might want to add a warning about trailing whitespace?

#Should add varlevels parameter, as alternative to a named matrix
as.w8target.matrix <- function(target.matrix, samplesize, varname, varlevels = NULL, byrow = TRUE){
  require(gdata) #for the "unmatrix" function
  
  ## ---- error handling ----
  if(sum(is.na(rownames(target.matrix))) > 0 | is.null(rownames(target.matrix))) warning("Matrix has invalid or missing row names")
  if(sum(is.na(colnames(target.matrix))) > 0 | is.null(colnames(target.matrix))) warning("Matrix has invalid or missing column names")
  
  ## ---- main function ----
  target.vector <- unmatrix(target.matrix, byrow = byrow)
  names(target.vector) <- gsub(":", ".", names(target.vector))
  target.counts <- (target.vector / sum(target.vector)) * samplesize
  
  w8target <- data.frame(names(target.counts), target.counts)
  names(w8target) <- c(varname, "Freq")
  
  class(w8target) <- c("w8target", "data.frame")
  return(w8target)
}

as.w8target.df <- function(target.df, samplesize, varname = NULL, varlevels = NULL){
  ## ---- error handling ----
  if(!("Freq" %in% names(target.df))) stop("data frames must have Freq column for conversion to w8target")
  if(ncol(target.df) > 2) stop("data frames must have two columns for converstion to w8target")
  
  ## ---- main function ----
  w8target <- target.df
  w8target$Freq <- (target.df$Freq / sum(target.df$Freq)) * samplesize
  
  if(!is.null(varLevels)) w8target[names(w8target) != "Freq"] <- varlevels
  if(!is.null(varname)) names(w8target)[names(w8target) != "Freq"] <- varname
  
  class(w8target) <- c("w8target", "data.frame")
  return(w8target)
}

as.w8target.numeric <- function(target.numeric, samplesize, varname, varlevels = NULL){
  ## ---- error handling ----
  if(is.null(varlevels)){
    if(sum(is.na(names(target.numeric))) > 0 | is.null(names(target.numeric))) stop("Vector has invalid or missing names - try specifying varlevels")
  } else{
    if(length(varlevels) != length(target.numeric)) stop("varlevels must be of length", length(target.numeric))
    names(target.counts) <- varlevels
  }
  
  ## ---- main function ----
  target.counts <- target.numeric * samplesize
  
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

#Input: "target_list" - a w8target object containg target data; "observed_data" - a column of observed factor data
#Output: a boolean, whether we think target_list and observed_data will be compatible; along with a warning message explainig the failure if FALSE is returned

#TO DO:
#Consider flagging trailing whitespace in target_list or levels(observed_data)
#allow a character or boolean observed_data column
#Check for empty levels in observed_data - these should return FALSE if there is a valid level in target_list, but possibly return TRUE if they are missing the matching level in target_list

checkTargetMatch <- function(target_list, observed_data){
  obs_levels <- levels(observed_data)
  
  #Check if if number of levels in observed data matches length of target
  if(length(target_list[,1]) != length(obs_levels)){
    warning("number of variable levels in observed factor variable does not match length of target")
    return(FALSE)
  }
  
  #Check for levels in observed data that do not match levels in target
  if(sum(target_list[,1] != obs_levels) > 0){
    #Check if number of levels is the same
    if(sum(sort(target_list[,1]) != sort(obs_levels)) == 0) warning("variable levels in observed factor variable are sorted differently from target")
    
    #Identify missing levels in both observed and target data
    missing_from_target.index <- !(target_list[,1] %in% obs_levels)
    missing_from_obs.index <- !(obs_levels %in% target_list[,1])
    missing_from_target.string <- paste(target_list[missing_from_target.index, 1], collapse = ", ")
    missing_from_obs.string <- paste(obs_levels[missing_from_obs.index], collapse = ", ")
    
    if(sum(missing_from_target.index) > 0) warning(paste("variable levels", missing_from_target.string, "in target are missing from observed factor variable"))
    if(sum(missing_from_obs.index) > 0) warning(paste("variable levels", missing_from_obs.string, "in observed factor vairable are missing from target"))
    
    return(FALSE)
  } else(return(TRUE))
}



## ==== QUICKRAKE ====

#This is the workhorse function - a wrapper for "rake" that is intended to take targets in a more flexible format
#However, flexibility also can be dangerous!

#I want to make substantial changes to this function, so that it coerces other weight target formats to a w8target object

#Input: "design", an svydesign object or else a data.frame that can be coerced to an svydesign object
# "weightVarList", a list or vector of characters, containing names of variables to be used in weighting (note that this specification allows us to pass a weightTargets object including more potential weighting variables than we are currently using)
# "weightTargets", a list of w8target objects (I want to change this so it takes a more flexible format)
# "weightTarget.id", a character  string that specificies whether we get the names of weight target variables from the named items of a list, or the columns of a data frame
# sampleSize - an integer with the desired post-weight sample size
#Output: a weighhted svydesign object

#TO DO
#Add a default value for sample size
#Add a default value for weightVarList - all the variable in weightTargets

#Should allow weightTargets in a variety of formats, and convert to w8target (including calling checkTargetMatch to ensure the conversion works)
#Consider adding a a "force" parameter to change the targets to match with observed data, based on "order" (force first observed level to match with first target level) or "name" (re-sort according to name)
#Think about ways to automatically handle minor problems with checkTargetMatch - trailing whitespace, differently sorted variable levels

quickRake <- function(design, weightVarList, weightTargets, sampleSize, weightTarget.id = "colname", ...){
  
  #CHECK IF TARGETS EXIST FOR WEIGHTING VARIABLES
  #Define whether weight targets are found based on the name of the list item, or the name of the second column of the data frame
  #Really they should be in both places, but we can take one or the other for flexibility
  if(weightTarget.id == "listname"){
    weight_target_names <- names(weightTargets)
    weightTargets <- mapply(function(w8target, varname){
      colnames(w8target)[1] <- varname
      return(w8target)
    })
  }else if(weightTarget.id == "colname"){
    weight_target_names <- sapply(weightTargets, function(onetarget) names(onetarget)[1])
    names(weightTargets) <- weight_target_names
  }else stop("invalid specification for weightTarget.id")
  
  missing_from_targets <- !(weightVarList %in% weight_target_names)
  if(sum(missing_from_targets) > 0) stop(paste("Targets were not found for weighting variables", toString(weightVarList[missing_from_targets], sep = ", ")))
  
  #Check if observed data exists for weighting variables
  if("data.frame" %in% class(design)){
    design <- svydesign(~0, data = design, control = list(verbose = FALSE))
  } 
  missing_from_observed <- !(weightVarList %in% names(design$variables))
  if(sum(missing_from_observed) > 0) stop(paste("Observed data was not found for weighting variables", toString(weightVarList[missing_from_observed], sep = ", ")))
  
  #Run weights
  sample.margins <- lapply((paste0("~", weightVarList)), as.formula)
  population.margins <- weightTargets[c(weightVarList)]
  
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


