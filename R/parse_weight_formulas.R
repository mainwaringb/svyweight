## Code
parse_weight_formulas <- function(..., data){
    arg_list <- list(...)
    
    # ---- Evaluate the formula function calls ----
    parsed_args <- simplify2array(lapply(arg_list, function(onearg){
        if(!("formula" %in% class(onearg))) error("Argument is not specified as a formula")
        data_call <- onearg[[2]]
        target_call <- onearg[[3]]
        
        data_object <- with(data, eval(data_call))
        target_object <- with(data, eval(target_call))
        variable_name <- as.character(onearg)[[2]]
        
        # Variables names with special characteres cause problems
        # For now we'll use gsub for a quick fix?
        variable_name <- gsub("\`", "", variable_name, fixed = TRUE)
        variable_name <- gsub(" ", "_", variable_name, fixed = TRUE)
        variable_name <- gsub("\'", "", variable_name, fixed = TRUE)
        variable_name <- gsub("\"", "", variable_name, fixed = TRUE)
        # If target stores a variable name, consider overwriting variable_name with this
        # Consider replacing backtick with quote in variable name, or add literal \
        
        # Need to check that the function call doesn't drop any rows!!!
        return(list(data_object = data_object, target_object = target_object, variable_name = variable_name))
    }))
    
    # ---- Reformat the output into separate data, target, and varnames object ----
    parsed_var_names <- unlist(parsed_args["variable_name",], recursive = FALSE)
    
    parsed_data <- data.frame(parsed_args["data_object",])
    names(parsed_data) <- parsed_var_names
    
    parsed_targets <- lapply(parsed_args["target_object",,drop = FALSE], function(x) x)
    names(parsed_targets) <- parsed_var_names
    
    # ---- Create an updated data frame ----
    # Append new columns
    new_var_names <- parsed_var_names[!(parsed_var_names %in% colnames(data))]
    out_data <- cbind(data, parsed_data[, new_var_names, drop = FALSE])
    
    out <- list(data = out_data, targets = parsed_targets)
    return(out)
}

# ## Basic test
# library(dplyr)
# age_recode_vec <- c("<=39" = .31, "40-49" = .15, 
#                     "50-59" = .19, "60-69" = .15, ">=70" = .21)
# 
# data_plus_targets <- parse_weight_formulas(
#         recode(agecat, `<=29` = "<=39", `30-39` = "<=39") ~ age_recode_vec, 
#         eastwest ~ c(`East Germany` = .805, `West Germany` = .195), 
#     data = gles17)
# 
# # This won't work if the variable names have nonstandard characters in them
# rakew8(data_plus_targets$data, data_plus_targets$targets)
# 
# # This works now
# names(data_plus_targets$data)[17] <- "age_rec"
# names(data_plus_targets$targets)[1] <- "age_rec"
# rakew8(data_plus_targets$data, data_plus_targets$targets)
