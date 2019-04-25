require("survey")

#===FUNCTIONS TO LOAD TARGETS FROM CSVS===

get_matrix_targets <- function(filepath, samplesize, varname = gsub(pattern = ".csv", replacement = "", x = filepath), encoding = "UTF-8"){

  target.matrix <- as.matrix(read.csv(filepath, row.names = 1, encoding = encoding))
  target.df <- as.w8target.matrix(target.matrix = target.matrix, samplesize = samplesize, varname = varname)
  
  return(target.df)
}

get_vector_targets <- function(filepath, samplesize, varname = gsub(pattern = ".csv", replacement = "", x = filepath)){
  target.df <- read.csv(filepath, header = FALSE, col.names = c(varname, "Freq"))

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
  
  return(w8target)
}

as.w8target.df <- function(target.df, samplesize, varname){
  w8target <- target.df
  w8target$Freq <- (target.df$Freq / sum(target.df$Freq)) * samplesize
  
  return(w8target)
}

as.w8target.vector <- function(target.vector, samplesize, varname){
  target.counts <- target.vector * samplesize
  
  w8target <- data.frame(names(target.counts), target.counts)
  names(w8target) <- c(varname, "Freq")
  
  return(w8target)
}


#===MISCELLANEOUS FUNCTIONS===

#Check whether the weight targets have the same length, and same levels, as the observed variable
#TO DO: check whether A) the *reordered* weight targets and observed variables levels match, or B) trimming whitespaces from strings can fix issues with levels
#This might ultimately be a separate function
checkTargetMatch <- function(target_list, observed_data){
  if(length(target_list[,1]) != length(levels(observed_data))) return(FALSE)
  if(sum(target_list[,1] != levels(observed_data)) > 0) return(FALSE) else(return(TRUE))
}

#TO DO: call checkTargetMatch, and return an error if targets don't match
#TO DO: check whether the sum of target weights for each variable is the same
easyRake <- function(design, weightVarList, weightTargets, ...){
  sample.margins <-  lapply((paste0("~", weightVarList)), as.formula)
  population.margins <- weightTargets[c(weightVarList)]
  
  if("data.frame" %in% class(design)){
    design <- svydesign(~0, data = design, control = list(verbose = FALSE))
  } 
  
  weighted <- rake(design = design, sample.margins = sample.margins, population.margins = population.margins, ...)
  return(weighted)
}

#Kish's approximate weighting efficiency
eff_n <- function(design){
  myweights <- weights(design)
  eff_n <- (sum(myweights) ^ 2) / (sum(myweights ^ 2))
  return(eff_n)
}

