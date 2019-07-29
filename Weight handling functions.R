require("survey")

#===FUNCTIONS TO LOAD TARGETS FROM CSVS===

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


#===FUNCTIONS TO CONVERT MATRICES, DFS, AND VECTORS TO W8 TARGETS===

as.w8target.matrix <- function(target.matrix, samplesize, varname, byrow = TRUE){
  require(gdata)
  target.vector <- unmatrix(target.matrix, byrow = byrow)
  names(target.vector) <- gsub(":", ".", names(target.vector))
  target.counts <- (target.vector / sum(target.vector)) * samplesize
  
  w8target <- data.frame(names(target.counts), target.counts)
  names(w8target) <- c(varname, "Freq")
  
  class(w8target) <- "w8target"
  return(w8target)
}

#Should add optional varname parameter, in case we want to change the varname
as.w8target.df <- function(target.df, samplesize){
  w8target <- target.df
  w8target$Freq <- (target.df$Freq / sum(target.df$Freq)) * samplesize
  
  class(w8target) <- "w8target"
  return(w8target)
}

as.w8target.vector <- function(target.vector, samplesize, varname){
  target.counts <- target.vector * samplesize
  
  w8target <- data.frame(names(target.counts), target.counts)
  names(w8target) <- c(varname, "Freq")
  
  class(w8target) <- "w8target"
  return(w8target)
}


#===MISCELLANEOUS FUNCTIONS===

#Check whether the weight targets have the same length, and same levels, as the observed variable
checkTargetMatch <- function(target_list, observed_data){
  obs_levels <- levels(observed_data)
  
  #Check if if length observed data matches length of target
  if(length(target_list[,1]) != length(obs_levels)){
    warning("number of variable levels in observed factor variable does not match length of target")
    return(FALSE)
  }
  
  #Check for missing levels in observed and target
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

#Should allow weightTargets in a variety of formats, and convert to w8target
#Add a "force" parameter to change the targets to match with observed data, based on "order" (force first observed level to match with first target level) or "name" (re-sort according to name)
#Plus consider adding a trimStrings parameter
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

#Kish's approximate weighting efficiency
eff_n <- function(design){
  myweights <- weights(design)
  eff_n <- (sum(myweights) ^ 2) / (sum(myweights ^ 2))
  return(eff_n)
}


