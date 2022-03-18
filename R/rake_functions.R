#major to-do list:
#nice to have
#' 
## ==== RAKESVY + RAKEW8 ====

#These is the workhorse functions for the package
#they are designed to allow flexible input formats for targets - However, flexibility also can be dangerous!

#TO DO
#Don't rename columns of data frames when converting to w8margin

#' Flexibly Calculate Rake Weights
#' @import survey
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
#' @param na.targets A characters string that specifies how to  handle NAs in *targets*,
#'   either "fail" (the default) or "observed" (see details). 
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
#' @details By default, with parameter \code{na.targets = "fail"}), an NA in weight targets 
#'    will cause an error. With \code{na.targets = "observed"}, rakesvy and rakew8 will instead
#'    compute a target that matches the observed data. The category with an NA target will 
#'    therefore have a similar incidence rate in the pre-raking and post-raking dataset.
#'    This is accomplished by calling \code{\link{impute_w8margin}} before raking; see
#'    the impute_w8margin documentation for more details. Note that NAs in \emph{observed}
#'    data (as opposed to targets) will always cause failure, and are not affected by this parameter.
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
                    samplesize = "from.data", match.levels.by = "name", na.targets = "fail",
                    rebase.tol = .01, control = list(maxit = 10, epsilon = 1, verbose = FALSE)){

    if(!any((c("data.frame", "survey.design") %in% class(design)))) stop("Invalid value for design; must be a data.frame or survey.design object")
    if("data.frame" %in% class(design)){
        #Notice that we are suppressing the warning here - svydesign will otherwise produce a warning that no input weights are provided
        suppressWarnings(design <- survey::svydesign(~0, data = design, control = list(verbose = FALSE)))
    } 
    
    w8 <- rakew8(design = design, ...,
                 samplesize = samplesize, 
                 match.levels.by = match.levels.by, rebase.tol = rebase.tol, control = control)
    design$prob <- 1/w8
    
    return(design)
}

#' @rdname rakesvy
#' @export
rakew8 <- function(design, ...,
                   samplesize = "from.data", match.levels.by = "name", na.targets = "fail",
                   rebase.tol = .01, control = list(maxit = 10, epsilon = 1, verbose = FALSE)){
    
    
    ## ==== HOUSEKEEPING ====
    
    # ---- Convert misc objects to needed classes ----
    # Convert ... to list 
    target_formulas <- list(...)
    # A a single list was passed to ... originally, then unlist it
    if(length(target_formulas) == 1 & "list" %in% class(target_formulas[[1]])) 
        target_formulas <- target_formulas[[1]]
    
    # Convert data frame to svydesign object
    if(!any((c("data.frame", "survey.design") %in% class(design)))) stop("Invalid value for design; must be a data.frame or survey.design object")
    if("data.frame" %in% class(design)){
        #Notice that we are suppressing the warning here - svydesign will otherwise produce a warning that no input weights are provided
        suppressWarnings(design <- survey::svydesign(~0, data = design, control = list(verbose = FALSE)))
    } 
    # ---- Check for valid values on inputs ----
    if(sum(!(match.levels.by %in% c("name", "order", "exact"))) > 0){
        stop("Invalid value(s) ", 
             paste(match.levels.by[!(match.levels.by %in% c("name", "order", "exact"))]),
             " in match.levels.by"
        )
    }
    if(!na.targets %in% c("fail", "observed") | length(na.targets) != 1) 
        stop('Invalid value for na.targets; must be one of "fail" or "observed"') # To add "weighted.observed" 
    na.targets.allow <- na.targets == "observed" # Value to be passed to w8margin_matched and as.w8margin
    
    # if match.levels.by is a scalar, repeat it for every variable
    if(length(match.levels.by) == 1) 
        match.levels.by <- rep(match.levels.by, length(target_formulas))
    else if(length(match.levels.by) != length(target_formulas)) 
        stop("Incorrect length for match.levels.by")
    if(any(!(match.levels.by %in% c("name", "order")))){
        stop('Invalid value(s) for match.levels.by; must be one of "name" or "order"')
    }
    
    if(!("numeric" %in% class(rebase.tol)) | rebase.tol > 1 | rebase.tol < 0){
        stop('Invalid value for rebase.tol; must be numeric between 0 and 1')
    }
    
    if(!("numeric" %in% class(samplesize) & samplesize > 0) & !(samplesize %in% c("from.data", "from.targets"))){
        stop('Invalid value for samplesize; must be numeric greater than 0 or one of "from.data" and "from.target')
    }
    
    ## ==== EVALUATE TARGETS ====
    
    # Extract targets from formula
    targets <- extractTargets(target_formulas)
    
    # Parse formulas to generate new data frame columns
    # And change any weight target names that conflict with existing variables
    
    isDataFrame <- sapply(targets, function(x) ("w8margin" %in% class(x) | "data.frame" %in% class(x)))
    parsed_out <- parseTargetFormulas(
        target_formulas = target_formulas, 
        weightTargetNames = getWeightTargetNames(
            targets = targets, 
            target_formulas = target_formulas, 
            isDataFrame = isDataFrame), 
        design = design
    )
    design <- parsed_out$design
    weightTargetNames <- parsed_out$weightVarNames
    
    targets <- setWeightTargetNames(
        weightTargetNames = weightTargetNames, 
        targets = targets, 
        isDataFrame = isDataFrame
    )
    
    # now that we have the names of weighting variables, convert the weight target variables to factors
    design$variables[,weightTargetNames] <- lapply(
        design$variables[, weightTargetNames, drop = FALSE], as.factor
    )
    
    
    ## ==== PROCESS TARGETS FOR COMPATIBILITY WITH DATA ====
    
    # ---- Get info we need in order to process  ----
    # Define sample size 
    if(samplesize == "from.data"){ #"from.data" means we want to take a centrally-specified sample size
        nsize <- sum(weights.survey.design(design))
    } else if(samplesize == "from.targets"){ #"from.targets" means we want to take the sample size found in the targets, IE not specify one here
        nsize <- NULL
    } else nsize <- samplesize
    
    # ---- Do the processing ----
    # Change target level names if match.levels.by = "order"
    # (this parameter setting tells us that the first row of the target should automatically be the first level of the variable, and so on)
    forcedLevels <- mapply(function(target, observed, varname, isForced){
            if(isForced == FALSE) 
                return(NULL)
            else{
                if(length(levels(observed)) == target.length(target))
                    return(levels(observed)) #If length of target matches levels of observed, return that
                else if(length(levels(factor(observed))) == target.length(target)) 
                    return(levels(factor(observed))) #If length doesn't match, see if refactoring to drop empty levels will help
                else 
                    stop("Length of target for variable '", varname, "' did not match number of levels in observed data")
            }
        }, 
        target = targets, 
        observed = design$variables[weightTargetNames], 
        varname = weightTargetNames, 
        isForced = match.levels.by == "order", 
        SIMPLIFY = FALSE
    )
    
    # save original samples sizes (for diagnostics later); a targetsum function with methods might be a better way to handle this
    origTargetSums <- mapply(function(targets, forcedLevels, weightTargetNames){
        sum(as.w8margin(
            target = targets, 
            varname = weightTargetNames,
            levels = forcedLevels, 
            na.allow = na.targets.allow)$Freq, na.rm = TRUE)
    }, targets = targets, forcedLevels = forcedLevels, weightTargetNames = weightTargetNames)
    
    # if we are getting sample sizes from targets, make sure the sizes are consistent
    if(samplesize == "from.targets" & length(origTargetSums) > 1){ 
        rebaseRatio <- origTargetSums[-1] / origTargetSums[1]
        isRebaseTolerated <- rebaseRatio > (1 - rebase.tol) & rebaseRatio < (1 + rebase.tol) #check if the ratio is 1 +- some tolerance
        if(!all(isRebaseTolerated)) 
            stop("Target sample sizes must be consistent when samplesize = 'from.targets'")
        nsize <- mean(origTargetSums)
    }
    
    #Convert targets to class w8margin
    targets <- mapply(
        as.w8margin, 
        target = targets, 
        varname = weightTargetNames, 
        na.allow = na.targets.allow, 
        levels = forcedLevels, #forcedLevels is NULL if we aren't forcing the level
         MoreArgs = list(samplesize = nsize), SIMPLIFY = FALSE)
    unimputedTargets <- targets
    
    # Impute NA values in targets
    # Generate initial list of which targets need imputation
    imputedTargetNames <- weightTargetNames[
        sapply(targets, function(onetarget){any(is.na(onetarget$Freq))})
    ]
    # NB: Should ideally add some tryCatch here so that, if there's a bad match between the observed and target here
    # NB: We don't produce an error here, but instead keep the NA weight target (and then produce an error below in w8margin_matched)
    if(na.targets.allow){
        targets[imputedTargetNames] <- mapply(
            impute_w8margin,
            w8margin = targets[imputedTargetNames],
            observed = design$variables[, imputedTargetNames, drop = FALSE],
            MoreArgs = list(weights = weights.survey.design(design), rebase = TRUE),
            SIMPLIFY = FALSE
        )
    }
    # Don't throw any errors here if na.targets.allow == FALSE
    # This will get caught by isTargetMatch below, so adding more error catching is redundant and adds problematic complexity    
    
    
    ## ==== PROCESS DATA BY DROPPING ZERO TARGETS ====
    
    # remove cases from the observed data
    zeroTargetLevels <- lapply(targets, function(onetarget) 
        as.character(onetarget[!is.na(onetarget$Freq) & onetarget$Freq == 0, 1])) #identify zero levels
    design <- dropZeroTargets(design = design, zeroTargetLevels = zeroTargetLevels)
    
    # remove levels from the w8margin object (both imputed and unimputed version)
    zeroTargetRows <- lapply(targets, function(onetarget){
        !is.na(onetarget$Freq) & onetarget$Freq == 0
    })
    targets <- mapply(function(target, drop) target[!drop,], target = targets, drop = zeroTargetRows, SIMPLIFY = FALSE)
    unimputedTargets <- mapply(function(target, drop) target[!drop,], target = unimputedTargets, drop = zeroTargetRows, SIMPLIFY = FALSE)
    
    # Check which targets needed "real" imputation
    # NB: In future versions of the package, unimputedTargets will be used for re-imputing NA targets at each raking iteration
    # NB: But if an imputed target is zero, we want to handle it here and then NOT re-impute at each iteration
    imputedTargetNames <- weightTargetNames[
        sapply(targets, function(onetarget){any(is.na(onetarget$Freq))})
    ]

    
    ## ==== CHECK THAT TARGETS ARE VALID ====
    
    #Check if targets currently match
    isTargetMatch <- mapply(
        w8margin_matched, 
        w8margin = targets, 
        observed = design$variables[, weightTargetNames, drop = FALSE], 
        MoreArgs = list(na.targets.allow = na.targets.allow)
    )
    #Check if targets would match after re-factoring (re-factoring might produce less helpful messages)
    suppressWarnings(isRefactoredMatch <- mapply(w8margin_matched, 
        w8margin = targets, observed = design$variables[, weightTargetNames, drop = FALSE],
        MoreArgs = list(na.targets.allow = na.targets.allow, refactor = TRUE)))
    #Solve issues that can be solved with refactoring, stop if refactoring can't solve issues
    if(any(!isRefactoredMatch)) stop(
        "Target does not match observed data on variable(s) ", 
        paste(weightTargetNames[!isTargetMatch], collapse = ", "), 
        "\n", 
        paste0(names(warnings()), collapse = ", ")
    )
    else if(any(!isTargetMatch)) 
        design$variables[,weightTargetNames][!isTargetMatch] <- lapply(
            design$variables[, weightTargetNames, drop = FALSE][!isTargetMatch], 
            factor
        )
    
    
    ## ==== CHECK FOR CONSISTENT SAMPLE SIZES ====
    
    #consider moving this earlier, right after we compute origSum
    #to handle objects that were *not* coerced to a set sample size, check that samplesize for each w8margin is the same
    finalTargetSums <- sapply(targets, function(x) sum(x$Freq))
    isSizeTolerated <- (finalTargetSums / nsize) < (1 + rebase.tol) & (finalTargetSums / nsize) > (1 - rebase.tol)
    if(any(isSizeTolerated == FALSE)) 
        stop("Target sample sizes vary by more than specified tolerance; try changing rebase.tol")
    
    #to handle objects that were coerced to a set sample size, check if any of the sample sizes required substantive rebasing
    rebaseRatio <- lapply(origTargetSums, function(origSum) c(1, 100, nsize) / origSum)
    #Compute the ratio of 1, 100, and the original sample size to OrigSum
    isRebaseTolerated <- sapply(rebaseRatio, function(ratio) 
        any((ratio > (1 - rebase.tol)) & (ratio < (1 + rebase.tol)))) #check if the ratio is 1 +- some tolerancee
    if(any(isRebaseTolerated == FALSE)) 
        warning("targets for variable ", toString(names(targets)[!isRebaseTolerated], sep = ", "), 
            " sum to ", toString(origTargetSums[!isRebaseTolerated], sep = ", "), " and were rebased")
    
    
    ## ==== RUN WEIGHTS ====
    
    # Compute weights for valid cases
    sample.margins <- lapply((paste0(" ~ \`", weightTargetNames, "\`")), stats::as.formula)
    population.margins <- targets
    weighted <- survey::rake(
        design = design, 
        sample.margins = sample.margins, 
        population.margins = population.margins, 
        control = control)
    
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
getWeightTargetNames <- function(targets, target_formulas, isDataFrame){
 
    weightTargetNames <- sapply(target_formulas, function(onearg){
        if(!("formula" %in% class(onearg))) stop("Weight target argument is not specified as a formula")
        
        # Check if formula has left-hand side and return NULL if it doesn't
        if(length(onearg) != 3) stop("Weight target formula ", onearg, " must have left-hand side")
        
        # If formula does have left hand-side, extract that side
        lhs_char <- paste0(deparse(onearg[[2]], backtick = TRUE), collapse = " ")
        # Replace special characters, except for "." and "_"
        lhs_char <- gsub("[+-/*\\^=<>&|!@$\"'`%{}(),~:\\ ]+", ".", lhs_char)
        lhs_char <- gsub("[][]+", ".", lhs_char)
        return(lhs_char)
    })    
    
    return(weightTargetNames)
}

# Renames targets, after using getWeightTargetNames to ensure a consistent format
# returns a modified targets object
setWeightTargetNames <- function(weightTargetNames, targets, isDataFrame){
    old_column_names <- lapply(targets[isDataFrame], function(w8margin) colnames(w8margin)[1])
    
    targets[isDataFrame] <- mapply(function(target, varname){ #for any targets that were originally in w8margin or data frame format: change column name to match list name, and generate a warning
        if(colnames(target)[1] != varname){
            colnames(target)[1] <- varname
        }
        return(target)
    }, target = targets[isDataFrame], varname = weightTargetNames[isDataFrame], SIMPLIFY = FALSE)
    
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
#   and a new subobject "keep_yn" with length equal to the original (pre-dropped) data frame, indicating which rows of the original data frame have been dropped
# currently generates a warning if this dropping process leads to all cases in a given (nonzero) level being dropped
dropZeroTargets <- function(design, zeroTargetLevels){
    design$keep_cases <- data.frame(index = rownames(design$variables), keep_yn = TRUE)
    weightTargetNames <- names(zeroTargetLevels) # Note that this is defined locally, not passed as a parameter
    
    if(any(sapply(zeroTargetLevels, length) > 0) | any(weights.survey.design(design) == 0)){
        #Identify cases that will be dropped because they belong to a zero target
        design$keep_cases$keep_yn <-
            rowSums(
                simplify2array(
                    mapply(
                        function(var, levelsToDrop, varname){
                            if(length(levelsToDrop) > 0){
                                factorLevels <- levels(as.factor(var))
                                isValidLevel <- levelsToDrop %in% factorLevels
                                if(any(!isValidLevel)) 
                                    warning("Empty target level(s) ", toString(levelsToDrop[!isValidLevel], sep = ", "), 
                                            " do not match with any observed data on variable ", varname) 
                                var %in% levelsToDrop
                            }
                            else rep(FALSE, length(var))
                        }, var = design$variables[weightTargetNames], levelsToDrop = zeroTargetLevels, varname = weightTargetNames
                    )
                )
            ) == 0
        
        #Identify cases that will be dropped because they have a design weight of zero
        design$keep_cases$keep_yn[weights.survey.design(design) == 0] <- FALSE
        
        #Check which factor levels have valid cases, before dropping cases
        predrop.tab <- lapply(design$variables[weightTargetNames], table)
        
        #drop cases
        design <- subset(design, design$keep_cases$keep_yn)
        
        #remove the unneeded factor levels
        design$variables[weightTargetNames] <- mapply(
            function(factorvar, levelsToDrop){
                if(length(levelsToDrop) > 0) 
                    factor(factorvar, levels = levels(factorvar)[!(levels(factorvar) %in% levelsToDrop)])
                else 
                    factorvar
            }, 
            factorvar = design$variables[weightTargetNames], 
            levelsToDrop = zeroTargetLevels, 
            SIMPLIFY = FALSE
        )
        
        #Check if we are accidentally losing all cases (on factor levels with valid targets) by dropping some casses
        postdrop.tab <- lapply(design$variables[weightTargetNames], table) #Table after dropping cases
        mapply(
            function(pre, post, levelsToDrop, varname){   #Compare tables before and after dropping
                pre <- pre[!(names(pre) %in% levelsToDrop)]
                post <- post[!(names(pre) %in% levelsToDrop)]
                
                lostAllCases <- post == 0 & pre != 0
                if(any(lostAllCases)) 
                    warning("All valid cases for ", varname, " level(s) ", 
                        toString(names(pre)[lostAllCases]), " had weight zero and were dropped")
        }, pre = predrop.tab, post = postdrop.tab, levelsToDrop = zeroTargetLevels, varname = weightTargetNames)
    } 
    
    return(design)
}

# copy of weights.survey.design from survey package
# survey package doesn't export this, so we need to recreate it here
weights.survey.design <- function(object,...){
    return(1/object$prob)
}



## Evaluate formulas used for weight targets
# Input:
#    # target_formulas - a single list of formulas
#       each formula should have a variable (to be created from the given survey design object) on the left-hand side
#       this formula must produce a single column, and not drop any rows
#       the name of a w8margin object (or something that can be coerced to one) on the right hand side
#       the w8margin object is searched for in the environment specified by formula
#   weightTargetNames - what we should name the column output by, unless there is a clash
#   design - a survey design object, which is intended to be weighted
# Output: a list with two elements
#   Design - svydesign object, including any columns not in the original svydesign object
#   weightVarNames - the names of columns to be used for weighting
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
        data_object <- stats::model.frame(
            data_call, 
            data = design$variables, 
            na.action = NULL,
            drop.unused.levels = FALSE
        )
       
        return(data_object)
    })
    
    # Set names of new variables we've created
    names(parsed_data.list) <- weightTargetNames
    
    # Drop null outputs and transform non-null outputs into dataframe
    parsed_data.list <- parsed_data.list[!sapply(parsed_data.list, is.null)]
    
    if(length(parsed_data.list) > 0){
        # Check validity of non-NULL output
        parsed_nrows <- sapply(parsed_data.list, nrow)
        if(any(parsed_nrows != nrow(design$variables))) 
            stop("Weight target formulas ", 
                 target_formulas[parsed_nrows != nrow(design$variables)], 
                 "do not produce expected ", 
                 nrow(design$variables), 
                 " cases", 
                 call. = FALSE)
        parsed_ncols <- sapply(parsed_data.list, ncol)
        if(any(parsed_ncols != 1)) 
            stop("Weight target formulas ", 
                 target_formulas[parsed_ncols !=1],  
                 " do not produce 1 column of target data", 
                 call. = FALSE)
    
        parsed_data.df <- data.frame(parsed_data.list)
        names(parsed_data.df) <- names(parsed_data.list)

        #Check for parsed data names that already exist in the dataset
        duplicate_names.yn <- names(parsed_data.df) %in% names(design$variables)
        duplicate_names.varnames <- names(parsed_data.df)[duplicate_names.yn]
        
        #Check whether duplicated variable names have the same content
        duplicate_contents.yn <- mapply(
            function(x, y){
                out <- isTRUE(all.equal(x, y))
            }, 
            x = parsed_data.df[, duplicate_names.yn, drop = FALSE], 
            y = design$variables[, duplicate_names.varnames, drop = FALSE]
        )
        
        # Define and rename conflicting names
        to_rename.varnames <- names(parsed_data.df)[duplicate_names.yn][!duplicate_contents.yn]
        if(length(to_rename.varnames) > 0){
            renamed.varnames <- paste0(".rakew8_", names(parsed_data.df)[duplicate_names.yn][!duplicate_contents.yn])
            weightTargetNames[match(to_rename.varnames, weightTargetNames)] <- renamed.varnames
            names(parsed_data.df)[match(to_rename.varnames, names(parsed_data.df))] <- renamed.varnames
        }
        # Merge variables together
        design$variables <- cbind(
            design$variables, 
            parsed_data.df[!(names(parsed_data.df) %in% names(design$variables))]
        )
    }
    
    out <- list(design = design, weightVarNames = weightTargetNames)
    return(out)
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
    
    # ---- Get argument speciftying name of targets ----
    target_rhs <- lapply(target_formulas, function(onearg){
        #Get svy object, from the environment specified by the formula
        # Get the right-hand side of the formula (third element if a left-hand side exists, second element otherwise)
        if(length(onearg) == 3){
            target_call <- onearg[[3]]
        } else target_call <- onearg[[2]]
        
        return(target_call)
    })
    
    # ---- Evaluate arguments in appropriate environments ----
    parsed_targets <- lapply(
        1:length(target_rhs), 
        function(x) 
            eval(
                target_rhs[[x]], 
                envir = environment(target_formulas[[x]]))
        )
    if(any(sapply(parsed_targets, is.null))) 
        stop(
            "Right-hand side of target(s) ", 
            paste0(target_rhs[sapply(parsed_targets, is.null)], collapse = ", "), 
            " is NULL or could not be found in specified environments"
        )
    
    return(parsed_targets)
}

