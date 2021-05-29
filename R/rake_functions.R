#major to-do list:
#nice to have
# rakesvy:
# 1) Don't rename columns of data frames when converting to w8margin


## ==== RAKESVY + RAKEW8 ====

#These is the workhorse functions for the package
#they are designed to allow flexible input formats for targets - However, flexibility also can be dangerous!

#TO DO
#Don't rename columns of data frames when converting to w8margin

#' Flexibly Calculate Rake Weights
#' @description Calculate rake weights on a data frame or
#'   \code{\link[survey]{svydesign}} object. Targets may be counts or
#'   percentages, in vector, matrix, data frame, or w8margin form. Before
#'   weighting, targets are converted to w8margins, checked for validity, and
#'   matched to variables in observed data, \code{rakesvy} returns a weighted
#'   \code{svydesign} object, while \code{rakew8} returns a vector of weights.
#' @param design An \code{\link[survey]{svydesign}} object, or a data frame that
#'   can be coerced to an svydesign object. When a data frame is coerced, the
#'   coercion assuming no clustering or design weighting.
#' @param ... Formulas specifying weight targets, with an object that can be coerced 
#'   to class w8margin (see \code{\link{as.w8margin}}) on the right-hand side, and 
#'   (optionally) a matching variable or transformation of it on the left-hand side.
#'   Objects that can be coerced to w8margin include named numeric vectors and matrices, 
#'   and data frames in the format accepted by \code{rake}.
#' @param samplesize Either a number specifying the desired post-raking sample
#'   size, or a character string "from.data" or "from.targets" specifying how to
#'   calculate the desired sample size (see details).
#' @param match.levels.by A character string that specifies how to match levels in
#'   the target with the observed data, either "name" (the default) or "order"
#'   (see details).
#' @param match.vars.by A character  string that specifies how elements of
#'   targets are matched with variables in design, either "formula.lhs" (the
#'   default) or "col.name" (see details).
#' @param rebase.tol Numeric between 0 and 1. If targets are rebased, and
#'   the rebased sample sizes differs from the original sample size by more than
#'   this percentage, generates a warning.
#' @param control Parameters passed to the \code{control} argument of \code{\link[survey]{rake}}, to control the details of convergence in weighting. 
#' @details rakesvy and rakew8 wrangles observed data and targets into compatible formats,
#'   before using \code{\link[survey]{rake}} to make underlying weighting calculations. The function matches weight targets to observed
#'   variables, cleans both targets and observed variables, and then checks the
#'   validity of weight targets (partially by calling
#'   \code{\link{w8margin_matched}}) before raking. It also allows a weight
#'   target of zero, and assigns an automatic weight of zero to cases on this target
#'   level.
#' @details Weight target levels can be matched with observed variable levels in
#'   two ways, specified via the \code{match.levels.by} parameter. "name" (the
#'   default) matches based on name, disregarding order (so a "male" level in
#'   the weight target will be matched with a "male" level in the observed
#'   data). "order" matches based on order, disregarding name (so the first
#'   level or row of the target will match with the first level of the observed
#'   factor variable).
#' @details The name of the final targets can come from one of two places
#'   specified via the \code{match.vars.by} parameter. The default,
#'   "formula.lhs", generates a name based on the left-hand side of the formula.
#'   The alternative, "col.name" specifies that the non-"Freq" column name of each 
#'   w8margin object should indicate a matching variable in the design object;
#'   this will only work for weight targets in a \code{w8margin} or
#'   \code{data.frame} format.
#' @details The desired sample size (in other words, the desired sum of weights
#'   after raking)  is specified via the \code{samplesize} parameter. This can
#'   be a numeric value. Alternatively, "from.data" specifies that the observed
#'   sample size before weighting (taken from \code{sum(weights(design))} if
#'   applicable, or \code{nrow} if not); "from.targets" specifies that the total
#'   sample sizes in target objects should be followed, and should only be used
#'   if all targets specify the same sample size.
#' @return \code{rakesvy} returns an \code{svydesign} object with rake weights applied. Any changes
#'   made the variables in \code{design} in order to call \code{rake}, such as
#'   dropping empty factor levels, are temporary and \emph{not} returned in the
#'   output object. 
#' @return \code{rakew8} returns a vector of weights. This avoids creating
#'   duplicated \code{svydesign} objects, which can be useful when calculating multiple
#'   sets of weights for the same data.
#' @example inst/examples/rake_examples.R
#' @export
rakesvy <- function(design, ...,
                    samplesize = "from.data", match.levels.by = "name", match.vars.by = "formula.lhs", rebase.tol = .01, 
                    control = list(maxit = 10, epsilon = 1, verbose = FALSE)){
    if("data.frame" %in% class(design)){
        #Notice that we are suppressing the warning here - svydesign will otherwise produce a warning that no input weights are provided
        suppressWarnings(design <- survey::svydesign(~0, data = design, control = list(verbose = FALSE)))
    } 
    
    w8 <- rakew8(design = design, ...,
                 samplesize = samplesize, 
                 match.levels.by = match.levels.by, match.vars.by = match.vars.by, rebase.tol = rebase.tol, control = control)
    design$prob <- 1/w8
    
    return(design)
}

#' @rdname rakesvy
#' @export
rakew8 <- function(design, ...,
                   samplesize = "from.data", match.levels.by = "name", match.vars.by = "formula.lhs", rebase.tol = .01, 
                   control = list(maxit = 10, epsilon = 1, verbose = FALSE)){
    
    
    ## ==== HOUSEKEEPING ====
    
    # ---- Check for valid values on inputs ----
    if(sum(!(match.levels.by %in% c("name", "order", "exact"))) > 0) stop("Invalid value(s) ", paste(match.levels.by[!(match.levels.by %in% c("name", "order", "exact"))])," in match.levels.by")
    if(sum(!(match.vars.by %in% c("col.name", "formula.lhs"))) > 0) stop("Invalid value(s) ", paste(match.vars.by[!(match.vars.by %in% c("col.name", "formula.lhs"))])," in match.vars.by")
    
    # ---- Convert misc objects to needed classes ----
    # Convert ... to list 
    target_formulas <- list(...)
    # A a single list was passed to ... originally, then unlist it
    if(length(target_formulas) == 1 & "list" %in% class(target_formulas[[1]])) target_formulas <- target_formulas[[1]]
    
    # Convert data frame to svydesign object
    if("data.frame" %in% class(design)){
        #Notice that we are suppressing the warning here - svydesign will otherwise produce a warning that no input weights are provided
        suppressWarnings(design <- survey::svydesign(~0, data = design, control = list(verbose = FALSE)))
    } 

    # if match.levels.by is a scalar, repeat it for every variable
    if(length(match.levels.by) == 1) match.levels.by <- rep(match.levels.by, length(target_formulas))
    else if(length(match.levels.by) != length(target_formulas)) stop("incorrect length for match.levels.by")
    
    # ---- Compute things we need ----
    # Define sample size 
    if(samplesize == "from.data"){ #"from.data" means we want to take a centrally-specified sample size
        nsize <- sum(weights.survey.design(design))
    } else if(samplesize == "from.targets"){ #"from.targets" means we want to take the sample size found in the targets, IE not specify one here
        nsize <- NULL
    } else nsize <- samplesize
    
    
    ## ==== EVALUATE TARGETS ====
    # Extract targets from formula
    targets <- extractTargets(target_formulas)
    
    # Rename weight targets
    isDataFrame <- sapply(targets, function(x) ("w8margin" %in% class(x) | "data.frame" %in% class(x)))
    weightTargetNames <- getWeightTargetNames(targets = targets, target_formulas = target_formulas, match.vars.by = match.vars.by, isDataFrame = isDataFrame)
    targets <- setWeightTargetNames(weightTargetNames = weightTargetNames, targets = targets, match.vars.by = match.vars.by, isDataFrame = isDataFrame)
    
    # Create any new columns that are specified in ... formulas
    parsed_data <- parseTargetFormulas(target_formulas = target_formulas, weightTargetNames = weightTargetNames, design = design)
    if(!is.null(parsed_data)){
        design$variables <- cbind(design$variables, parsed_data[!(names(parsed_data$data) %in% names(design$variables))])
    }

    # now that we have the names of weighting variables, convert the weight target variables to factors
    design$variables[,weightTargetNames] <- lapply(design$variables[, weightTargetNames, drop = FALSE], as.factor)
    
    
    ## ==== PROCESS TARGETS FOR COMPATIBILITY WITH DATA ====
    
    # #Check if target exists for weighting variables
    # missing_from_observed <- !(weightTargetNames %in% names(design$variables))
    # if(sum(missing_from_observed) > 0) stop(paste("Observed data was not found for weighting variables ", toString(weightTargetNames[missing_from_observed], sep = ", ")))
    
    # Change target level names if match.levels.by = "order"
    # (this parameter setting tells us that the first row of the target should automatically be the first level of the variable, and so on)
    forcedLevels <- mapply(function(target, observed, varname, isForced){
        if(isForced == FALSE) return(NULL)
        else{
            if(length(levels(observed)) == target.length(target)) return(levels(observed)) #If length of target matches levels of observed, return that
            else if(length(levels(factor(observed))) == target.length(target)) return(levels(factor(observed))) #If length doesn't match, see if refactoring to drop empty levels will help
            else stop("Length of target for variable '", varname, "' did not match number of levels in observed data")
        }
    }, target = targets, observed = design$variables[weightTargetNames], varname = weightTargetNames, isForced = match.levels.by == "order", SIMPLIFY = FALSE)
    
    # save original samples sizes (for diagnostics later); a targetsum function with methods might be a better way to handle this
    origTargetSums <- mapply(function(targets, forcedLevels, weightTargetNames){
        sum(as.w8margin(target = targets, varname = weightTargetNames, levels = forcedLevels)$Freq)
    }, targets = targets, forcedLevels = forcedLevels, weightTargetNames = weightTargetNames)
    
    # if we are getting sample sizes from targets, make sure the sizes are consistent
    if(samplesize == "from.targets" & length(origTargetSums) > 1){ 
        rebaseRatio <- origTargetSums[-1] / origTargetSums[1]
        isRebaseTolerated <- rebaseRatio > (1 - rebase.tol) & rebaseRatio < (1 + rebase.tol) #check if the ratio is 1 +- some tolerance
        if(!all(isRebaseTolerated)) stop("Target sample sizes must be consistent when samplesize = 'from.targets'")
        nsize <- mean(origTargetSums)
    }
    
    #Convert targets to class w8margin
    targets <- mapply(as.w8margin, target = targets, varname = weightTargetNames, levels = forcedLevels,
                      MoreArgs = list(samplesize = nsize), SIMPLIFY = FALSE)
    
    
    ## ==== PRCOESS DATA BY DROPPING ZERO TARGETS ====
    
    # remove levels with targets of "0"
    # cases should be removed from the observed data, and levels should be removed from the w8margin object
    zeroTargetLevels <- lapply(targets, function(onetarget) as.character(onetarget[!is.na(onetarget$Freq) & onetarget$Freq == 0, 1])) #identify zero levels
    targets <- lapply(targets, function(onetarget) onetarget[is.na(onetarget$Freq) | onetarget$Freq != 0, ]) #drop zero levels from targets
    design <- dropZeroTargets(design = design, zeroTargetLevels = zeroTargetLevels)
    
    
    ## ==== CHECK THAT TARGETS ARE VALID ====
    
    #Check if targets currently match
    isTargetMatch <- mapply(w8margin_matched, w8margin = targets, observed = design$variables[, weightTargetNames, drop = FALSE])
    #Check if targets would match after re-factoring (re-factoring might produce less helpful messages)
    suppressWarnings(isRefactoredMatch <- mapply(w8margin_matched, w8margin = targets, observed = design$variables[, weightTargetNames, drop = FALSE],
                                                 refactor = TRUE))
    #Solve issues that can be solved with refactoring, stop if refactoring can't solve issues
    if(any(!isRefactoredMatch)) stop("Target does not match observed data on variable(s) ", paste(weightTargetNames[!isTargetMatch], collapse = ", "))
    else if(any(!isTargetMatch)) design$variables[,weightTargetNames][!isTargetMatch] <- lapply(design$variables[, weightTargetNames, drop = FALSE][!isTargetMatch], factor)
    
    
    ## ==== CHECK FOR CONSISTENT SAMPLE SIZES ====
    
    #consider moving this earlier, right after we compute origSum
    #to handle objects that were *not* coerced to a set sample size, check that samplesize for each w8margin is the same
    finalTargetSums <- sapply(targets, function(x) sum(x$Freq))
    isSizeTolerated <- (finalTargetSums / nsize) < (1 + rebase.tol) & (finalTargetSums / nsize) > (1 - rebase.tol)
    if(any(isSizeTolerated == FALSE)) stop("Target sample sizes vary by more than specified tolerance; try changing rebase.tol")
    
    #to handle objects that were coerced to a set sample size, check if any of the sample sizes required substantive rebasing
    rebaseRatio <- lapply(origTargetSums, function(origSum) c(1, 100, nsize) / origSum)
    #Compute the ratio of 1, 100, and the original sample size to OrigSum
    isRebaseTolerated <- sapply(rebaseRatio, function(ratio) any((ratio > (1 - rebase.tol)) & (ratio < (1 + rebase.tol)))) #check if the ratio is 1 +- some tolerancee
    if(any(isRebaseTolerated == FALSE)) warning("targets for variable ", toString(names(targets)[!isRebaseTolerated], sep = ", "), " sum to ", toString(origTargetSums[!isRebaseTolerated], sep = ", "), " and were rebased")
    
    
    ## ==== RUN WEIGHTS ====
    
    # Compute weights for valid cases
    sample.margins <- lapply((paste0(" ~ \`", weightTargetNames, "\`")), stats::as.formula)
    population.margins <- targets
    weighted <- survey::rake(design = design, sample.margins = sample.margins, population.margins = population.margins, control = control)
    
    # Merge valid case weights with zero weights
    design$keep_cases$weight <- 0
    design$keep_cases$weight[design$keep_cases$keep_yn == TRUE] <- weights.survey.design(weighted)
    
    return(design$keep_cases$weight)
}


## ==== INTERNAL FUNCTIONS ====

#Gets weight target names, which can be contained in one of two places:
# A) the left-hand side of a formula, applicable even if we use as.w8margin to convert target types
# B) the name of the second column of a w8margin object, applicable only if targets are class w8margin or data frame
# Returns a vector WeightTargetNames
getWeightTargetNames <- function(targets, target_formulas, match.vars.by, isDataFrame){
    if(match.vars.by == "formula.lhs"){
        weightTargetNames <- sapply(target_formulas, function(onearg){
            if(!("formula" %in% class(onearg))) stop("Weight target argument is not specified as a formula")
            
            # Check if formula has left-hand side and return NULL if it doesn't
            if(length(onearg) != 3) stop("Weight target formula ", onearg, " must have left-hand side")
            
            # If formula does have left hand-side, extract that side
            lhs_char <- as.character(onearg[[2]])
            # Replace special characters
            lhs_char <- gsub("[[:punct:]]+", ".", lhs_char)
            return(lhs_char)
        })
    # }else if(match.vars.by == "object.name"){
    #     weightTargetNames <- names(targets) #set weightTargetNames convenience variables to equal the list names
    #     if(length(unique(weightTargetNames)) < length(targets)){
    #         if(is.null(weightTargetNames)) stop("List of weight targets must be named unless match.vars.by is set to 'col.name'")
    #         if(sum(weightTargetNames == "") > 0) stop("One or more weight target names is blank")
    #         stop("Duplicated weight targets names", paste(weightTargetNames[duplicated(weightTargetNames)], sep = ", " ))
    #     }
    # 
    }else if(match.vars.by == "col.name"){
        if(any(!(isDataFrame | sapply(targets, function(x) "data.frame" %in% class(x))))) stop("match.vars.by = 'col.name' requires targets of class w8margin or data.frame")
        
        weightTargetNames <- sapply(targets, function(onetarget) names(onetarget)[1])
        doesNotMatch <- names(targets) != weightTargetNames
    }
    
    return(weightTargetNames)
}

# Renames targets, after using getWeightTargetNames to ensure a consistent format
# returns a modified targets object
setWeightTargetNames <- function(weightTargetNames, targets, match.vars.by, isDataFrame){
    old_column_names <- lapply(targets[isDataFrame], function(w8margin) colnames(w8margin)[1])
    
    if(match.vars.by == "formula.lhs"){
        targets[isDataFrame] <- mapply(function(target, varname){ #for any targets that were originally in w8margin or data frame format: change column name to match list name, and generate a warning
            if(colnames(target)[1] != varname){
                colnames(target)[1] <- varname
            }
            return(target)
        }, target = targets[isDataFrame], varname = weightTargetNames[isDataFrame], SIMPLIFY = FALSE)
    } 
    names(targets) <- weightTargetNames
    return(targets)
}

# Removes rows from the dataset that have a weight of zero
# Either because their design weight is zero, or because they belong to a group where the target is zero
# Inputs:
#   design: an svydesign object, where we want to drop cases from
#   zeroTargetLevels: a list, where each named element is a character vector indicating which levels of a variable should be dropped
#     eg, list(agecat = c("under 18", "90+"), gender = c(), education = c("Don't Know"))
# Returns:
#   a modified svydesign object, with some cases dropped from "variables" subobject 
#   and a new subject "keep_yn" with length equal to the original (pre-dropped) data frame, indicating which rows of the original data frame have been dropped
# currently generates a warning if this dropping process leads to all cases in a given (nonzero) level being dropped
dropZeroTargets <- function(design, zeroTargetLevels){
    design$keep_cases <- data.frame(index = rownames(design$variables), keep_yn = TRUE)
    weightTargetNames <- names(zeroTargetLevels) # Note that this is defined locally, not passed as a parameter
    
    if(any(sapply(zeroTargetLevels, length) > 0) | any(weights.survey.design(design) == 0)){
        #Identify cases that will be dropped because they belong to a zero target
        design$keep_cases$keep_yn <-
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
        
        #Identify cases that will be dropped because they have a design weight of zero
        design$keep_cases$keep_yn[weights.survey.design(design) == 0] <- FALSE
        
        #Check which factor levels have valid cases, before dropping cases
        predrop.tab <- lapply(design$variables[weightTargetNames], table)
        
        #drop cases
        design <- subset(design, design$keep_cases$keep_yn)
        
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
    
    return(design)
}

# copy of weights.survey.design from survey package
# survey package doesn't export this, so we need to recreate it here
weights.survey.design <- function(object,...){
    return(1/object$prob)
}

# copy of model.frame.survey.design from survey package
# as this isn't exported 


## Evaluate formulas used for weight targets
# Input:
#    # target_formulas - a single list of formulas
#       each formula should have a variable (to be created from the given survey design object) on the left-hand side
#       this formula must produce a single column, and not drop any rows
#       the name of a w8margin object (or something that can be coerced to one) on the right hand side
#       the w8margin object is searched for in the environment specified by formula
#   weightTargetNames - what we should name the column output by 
#   design - a survey design object, which is intended to be weighted
# Output: a list with two elements
#   Data - parsed data frame (omitting any target formulas that do not have a left-hand side to evaluate)
#   Varnames - a vector with the names given to any parsed variables, interspersed with NULL for target formulas with no left-hand side
parseTargetFormulas <- function(target_formulas, weightTargetNames, design){
    # ---- Convert to list, in case there is only one formula ----
    if(!("list" %in% class(target_formulas))) target_formulas <- list(target_formulas)
    
    # ---- Evaluate the formula function calls ----
    parsed_data.list <- lapply(target_formulas, function(onearg){
        if(!("formula" %in% class(onearg))) stop("Weight target argument is not specified as a formula")
        
        # Check if formula has left-hand side and return NULL if it doesn't
        if(length(onearg) != 3) return(NULL)
        
        # If formula does have left hand-side, extract that side
        data_call <- onearg[-3]
        data_object <- stats::model.frame(data_call, data = design$variables, na.action = NULL, drop.unused.levels = FALSE)
       
        return(data_object)
    })
    
    # Set names of new variables we've created
    names(parsed_data.list) <- weightTargetNames
    
    # Drop null outputs, and return NULL if there are no non-null outputs
    parsed_data.list <- parsed_data.list[!sapply(parsed_data.list, is.null)]
    if(length(parsed_data.list) == 0){
        parsed_data.df <- NULL
    } else{
        # Check validity of non-NULL output
        parsed_nrows <- sapply(parsed_data.list, nrow)
        if(any(parsed_nrows != nrow(design$variables))) stop ("Weight target formulas ", target_formulas[parsed_nrows != nrow(design$variables)], "do not produce expected ", nrow(design$variables), " cases")
        parsed_ncols <- sapply(parsed_data.list, ncol)
        if(any(parsed_ncols != 1)) stop ("Weight target formulas ", target_formulas[parsed_ncols !=1],  " do not produce 1 column of target data")
    
        parsed_data.df <- data.frame(parsed_data.list)
        names(parsed_data.df) <- names(parsed_data.list)
    }
    return(parsed_data.df)
}

# Get targets from environment
# Technically speaking, rakew8 gets passed formulas that specify the *name* of targets
# And the environment they're in
# This function pulls the targets based on name and environment
# Input:
#   target_formulas: a list of one or more formulas, where the right-hand side of the formula names an object
# Output: an object (that can presumably be coerced to class w8margin, although we don't check that here)

extractTargets <- function(target_formulas){
    # ---- Convert to list, in case there is only one formula ----
    if(!("list" %in% class(target_formulas))) target_formulas <- list(target_formulas)
    
    # ---- Get targets ----
    target_rhs <- lapply(target_formulas, function(onearg){
        #Get svy object, from the environment specified by the formula
        # Get the right-hand side of the formula (third element if a left-hand side exists, second element otherwise)
        if(length(onearg) == 3){
            target_call <- onearg[[3]]
        } else target_call <- onearg[[2]]
        
        return(target_call)
    })
    
    parsed_targets <- lapply(target_rhs, function(x) eval(x, envir = environment(x)))
    if(any(sapply(parsed_targets, is.null))) stop("Right-hand side of target(s) ", paste0(target_rhs[sapply(parsed_targets, is.null)], collapse = ", "), " is NULL or could not be found in specified environments")
    
    return(parsed_targets)
}

