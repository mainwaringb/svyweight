library(Rakehelper)
library(testthat)


## ==== To Do ====
# Test sample size and rebase.tol parameters



## ==== CHECK BASIC FUNCTIONALITY ====

# ---- Central case for tests, importing targets already in w8margin form ----
# Ensure that results haven't changed over time

test_that("rakew8 expected weights are generated using basic common parameters", {
    expect_equal( 
        rakew8(gles17,
               targets = targets_main.w8margin,
               match.vars.by = "listname",
               match.levels.by = "name"), 
        Rakehelper:::benchmark_out
    )
    
    expect_equal(
        rakew8(gles17, 
               targets = targets_main.w8margin, 
               match.vars.by = "colname",
               match.levels.by = "name"), 
        Rakehelper:::benchmark_out
    )
    
    expect_equal( 
        rakew8(gles17,
               targets = targets_main.w8margin,
               match.vars.by = "listname",
               match.levels.by = "order"), 
        Rakehelper:::benchmark_out
    )
    
    expect_equal(
        rakew8(gles17, 
               targets = targets_main.w8margin, 
               match.vars.by = "colname",
               match.levels.by = "order"), 
        Rakehelper:::benchmark_out
    )
})

# ---- Check that default parameters work as expected ----
test_that("rakew8 default parameters behave as expected", {
    expect_equal( # Should generate an error if, for some reason, we are defaulting to match.vars.by = "varname"
        suppressWarnings(rakew8(gles17, 
               targets = bad_colnames.w8margin)),
        suppressWarnings(rakew8(gles17, 
               targets = bad_colnames.w8margin,
               match.vars.by = "listname"))
    )
    
    expect_equal( #Should generate incorrect/unmatched results if for some reason defaulting to match.levels.by = "order"
        rakew8(gles17,
               targets = targets_reorder.w8margin
        ),
        rakew8(gles17,
               targets = targets_reorder.w8margin,
               match.levels.by = "name")
    )
    
    expect_equal( #Targets have sample size of 1.0, so if default goes to "from.targets" the weights won't match
        rakew8(gles17,
               targets = targets_reorder.w8margin
        ),
        rakew8(gles17,
               targets = targets_reorder.w8margin,
               samplesize = "from.data")
    )
})

# ---- Check basic w8margin conversions ----
# Arguably we should develop more direct ways to test whether rakew8 calls to w8margin work as expected
# might involve creating a separate function for the "Convert targets to class w8margin" section and testing this
# plus tests on the generation of forcedTargetLevels
# (Also should develop separate unit testing for the w8margin function)

test_that("rakew8 converts named list to w8margin objects correctly, with simple sample size settings", {
    expect_equal(
        rakew8(gles17, 
               targets = list(
                   vote2013 = targets.vec$vote2013, 
                   eastwest = targets.vec$eastwest_reorder, 
                   gender = targets.vec$gender),
               samplesize = "from.data"
               ),
        rakew8(gles17,
               targets = targets_reorder.w8margin,
               samplesize = "from.data")
    )
    
    expect_equal(
        rakew8(gles17, 
               targets = list(
                   vote2013 = targets.vec$vote2013, 
                   eastwest = targets.vec$eastwest_reorder, 
                   gender = targets.vec$gender),
               samplesize = 1000
        ),
        rakew8(gles17,
               targets = targets_reorder.w8margin,
               samplesize = 1000)
    )
    
    expect_equal(
        rakew8(gles17, 
               targets = list(
                   vote2013 = targets.vec$vote2013, 
                   eastwest = targets.vec$eastwest_reorder, 
                   gender = targets.vec$gender),
               samplesize = "from.targets" # these targets sum to 1.0, so we wouldn't really use them for sample size data
        ),
        rakew8(gles17,
               targets = targets_reorder.w8margin,
               samplesize = "from.targets")
    )
    
})


## ==== UNUSUAL TARGETS ====

#----only one target variable ----
# We want to test all these slightly different formulations, to ensure that lists arent getting dropped to vector
test_that("rakew8 correctly handles calls with only one weighting variable", {
    # Named list of 1 w8margin object - Expected pass
    expect_equal(
        rakew8(gles17, targets = list(vote2013 = targets_main.w8margin$vote2013), match.vars.by = "listname"),
        benchmark_onevar_out
    )
    expect_equal(
        rakew8(gles17, targets = list(targets_main.w8margin$vote2013), match.vars.by = "colname"),
        benchmark_onevar_out
    )
    
    # list of 1 vector - pass only if vector is named
    expect_equal( #Expected pass (named vector input)
        rakew8(gles17, targets = list(vote2013 = targets.vec$vote2013), match.vars.by = "listname"), # Named vector
        benchmark_onevar_out
    )
    expect_error( #Expected error (unnamed vector input)
        rakew8(gles17, targets = list(targets.vec$vote2013), match.vars.by = "listname"), 
        regexp = "List of weight targets must be named unless match.vars.by is set to 'colnames'",
        fixed = TRUE
    )
    
    # single vector, Expected error (make sure that a vector doesn't accidentally get accepted)
    expect_error(
        rakew8(gles17, targets = targets.vec$vote2013, match.vars.by = "listname"),
        "List of weight targets must be named unless match.vars.by is set to 'colnames'",
        fixed = TRUE
    )
    expect_error(
        rakew8(gles17, targets = targets.vec$vote2013, match.vars.by = "colname"),
        "match.vars.by = 'colname' requires targets of class w8margin",
        fixed = TRUE
    )
})

#----targets of 0% ----
# Check ultimate output from rakew8
test_that("rakew8 correctly handles target levels with a target of zero", {
    # Check that zero weights are included in dataset, rather than dropped
    # IE, the length of the output vector should be the same as nrow of the input data frame
    expect_length(
        rakew8(gles17, targets = targets_zero.w8margin), 
        nrow(gles17)
    )
})

# Check internal handling by dropZeroTargets
# This is not the most elegant at present
test_that("dropZeroTargets is dropping correct cases and refactoring", {
    # Dropping based on zero design weights - check if any weights are nonzero after dropping
    # Here, all cases with vote2013 == INELIGIBLE had design weights of zero and were dropped
    expect_false(
        expect_warning(
            any(
                weights(Rakehelper:::dropZeroTargets(gles17_zero_dweight.svy, zeroTargetLevels = list(vote2013 = c(), eastwest = c(), gender = c())))
                == 0),
            regexp = "All valid cases for vote2013 level(s) INELIGIBLE had weight zero and were dropped",
            fixed = TRUE
        )
    )
    
    # Dropping based on a target of 0% for vote2013: UNKNOWN/INELIGIBLE- check if any cases with vote2013 = UNKNOWN or INELIGIBLE
    # Here, all cases with eastwest = East Germany were dropped because they also had vote2013 = UNKNOWN
    expect_false(
        expect_warning(
            any(
                Rakehelper:::dropZeroTargets(gles17_bad_level.svy, zeroTargetLevels = list(vote2013 = c("UNKNOWN", "INELIGIBLE"), eastwest = c(), gender = c()))$variables$vote2013 
                %in% c("UNKNOWN", "INELIGIBLE")),
            regexp = "All valid cases for eastwest level(s) East Germany had weight zero and were dropped",
            fixed = TRUE
        )
    )
    
    # Dropping based on both criteria - check that length of returned object is correct
    expect_equal(
        nrow(Rakehelper:::dropZeroTargets(gles17_zero_dweight.svy, zeroTargetLevels = list(vote2013 = c("UNKNOWN", "INELIGIBLE"), eastwest = c("East Germany"), gender = c()))$variables),
        nrow(gles17[!(gles17$vote2013 %in% c("UNKNOWN", "INELIGIBLE")) & !gles17$eastwest == "East Germany" & weights(gles17_zero_dweight.svy) != 0,])
    )
    
    # Dropping based on targets of 0% - returned object has retained the correct cases
    expect_false(
        any(Rakehelper:::dropZeroTargets(gles17_zero_dweight.svy, zeroTargetLevels = list(vote2013 = c("UNKNOWN", "INELIGIBLE"), eastwest = "East Germany",  gender = c()))$variables$vote2013 
            %in% c("UNKNOWN", "INELIGIBlE"))
        |
            any(Rakehelper:::dropZeroTargets(gles17_zero_dweight.svy, zeroTargetLevels = list(vote2013 = c("UNKNOWN", "INELIGIBLE"), eastwest = "East Germany", gender = c()))$variables$eastwest
                == "East Germany")
    )
})

# Check errors and warnings for fringe cases
test_that("rakew8 generates appropriate errors and warnings", {
    # Error when one level has all zero design weights
    expect_warning(
        expect_error(
            rakew8(gles17_zero_dweight.svy, targets_main.w8margin),
            regexp = "Target does not match observed data on variable(s) vote2013",
            fixed = TRUE
        ),
        regexp = "All valid cases for vote2013 level(s) INELIGIBLE had weight zero and were dropped",
        fixed = TRUE
    )
    
    # Error when one level is lost when another variable is dropped due to zero target 
    expect_warning(
        expect_error(
            rakew8(gles17_bad_level.df, targets_zero.w8margin),
            regexp = "Target does not match observed data on variable(s) eastwest",
            fixed = TRUE
        ),
        regexp = "All valid cases for eastwest level(s) East Germany had weight zero and were dropped",
        fixed = TRUE
    )
    
    # Error when zero target is specified on invalid level
    expect_warning(
        rakew8(gles17, targets = bad_zero_level.w8margin),
        "Empty target level(s) ASDF do not match with any observed data on variable vote2013",
        fixed = TRUE
    )
    
})

# ---- NA in targets ----
# expected error
# error in as.w8margin.numeric
test_that("rakew8 appropriately handles NA targets", {
    expect_error(
        rakew8(gles17, targets = list(eastwest = targets.vec$eastwest, gender = targets.vec$gender,
                                      vote2013 = targets.vec$vote2013_na)),
        regexp = "Target is NA for level(s) INELIGIBLE, UNKNOWN, ",
        fixed = TRUE
    )
})


## ==== UNUSUAL OBSERVED VARIABLES ====

# ---- Observed variable levels with zero cases ----
test_that("rakew8 handles observed data with empty levels", {
    #CASE 1: OBSERVED DATA LEVEL WITH ZERO CASES, HAS (NON-ZERO) TARGET: error
    expect_warning(
        expect_error(
            rakew8(no_unknowns_10cat.df, targets_main.w8margin),
            regexp = "Target does not match observed data on variable(s) vote2013",
            fixed = TRUE
        ),
        regexp = "Observed data for vote2013 contains empty factor level UNKNOWN",
        fixed = TRUE
    )
    
    #CASE 2: OBSERVED DATA LEVEL WITH ZERO CASES, ZERO TARGET
    # Pass
    expect_equal(
        rakew8(no_unknowns_10cat.df, targets_zero.w8margin),
        rakew8(no_unknowns_9cat.df, targets_known.w8margin)
    )
    
    #CASE 3: LEVEL WITH  ZERO CASES, NO TARGET 
    # see first test in non-matching target and observed levels
})

# ---- Observed variables with NAs ----
# Have not fully thought through how NA targets *levels* should be handled
# Banning them seems simplest
# Possibly add an error that prohibits a factor level named NA
test_that("rakew8 handles NAs in dataset appropriately", {
    # NA in data (without NA factor level), no NA in target
    expect_warning(
        expect_error(
            rakew8(implicit_na.df, targets_main.w8margin),
            regexp = "Target does not match observed data on variable(s) eastwest",
            fixed =  TRUE
        ),
        regexp = "NAs in observed data for eastwest",
        fixed = TRUE
    )
    
    # NA in data (with NA factor level), no NA target
    expect_warning(
        expect_error(
            rakew8(explicit_na.df, targets_main.w8margin),
            regexp = "Target does not match observed data on variable(s) eastwest",
            fixed =  TRUE
        ),
        regexp = "Number of variable levels in observed data does not match length of target eastwest",
        fixed = TRUE
    )
    
    # NA in data (without NA factor level), implicit NA in target
    expect_warning(
        expect_error(
            rakew8(implicit_na.df, implicit_zero_target.w8margin),
            regexp = "Target does not match observed data on variable(s) eastwest",
            fixed = TRUE
        ),
        regexp = "NAs in observed data for eastwest",
        fixed = TRUE
    )
    
    # NA in data (with NA factor level), implicit NA in target
    # NOTE: UNINFORMATIVE ERROR MESSAGE HERE, SHOULD BE FIXED
    expect_error(
        rakew8(explicit_na.df, implicit_zero_target.w8margin),
        regexp = "Target does not match observed data on variable(s)",
        fixed = TRUE
    )
    
    # NA in data (without NA factor level), explicit NA in target
    expect_warning(
        expect_error(
            rakew8(implicit_na.df, explicit_zero_target.w8margin),
            regexp = "Target does not match observed data on variable(s) eastwest",
            fixed = TRUE
        ),
        regexp = "NAs in observed data for eastwest",
        fixed = TRUE
    )
    
    # NA in data (with NA factor level), explicit NA in target
    # NOTE: UNINFORMATIVE ERROR MESSAGE HERE, SHOULD BE FIXED
    # NOTE: ALSO, SHOULD THIS WORK????
    expect_error(
        rakew8(explicit_na.df, explicit_zero_target.w8margin),
        regexp = "Target does not match observed data on variable(s)",
        fixed = TRUE
    )
    
})

#---- samplesize and rebasetolerance NEED TESTS ----


## ==== HELPER FUNCTIONS ====

#----Targets where column names clash with list names----
test_that("getWeightTargetNames correctly resolves clash between target column name and target list name", {
    # listname
    expect_identical(
        Rakehelper:::getWeightTargetNames(bad_colnames.w8margin, match.vars.by = "listname", isw8margin = c(TRUE,TRUE,TRUE)),
        c("vote2013", "eastwest", "gender")
    )
    
    # colname
    expect_warning(
        expect_identical(
            as.vector(Rakehelper:::getWeightTargetNames(bad_colnames.w8margin, match.vars.by = "colname", isw8margin = c(TRUE,TRUE,TRUE))),
            c("pastvote", "eastwest", "gender")
        ),
        regexp = "target column name(s) pastvote do not match list name(s) vote2013; coercing to match column name",
        fixed = TRUE
    )
})

test_that("setWeightTargetNames correctly renames weight targets", {
    #listname
    expect_warning(
        expect_identical(
            Rakehelper:::setWeightTargetNames(weightTargetNames = c("vote2013", "eastwest", "gender"), targets = bad_colnames.w8margin, match.vars.by = "listname", isw8margin = c(TRUE,TRUE,TRUE)),
            targets_main.w8margin
        ),
        regexp = "w8margin column name(s) pastvote do not match list name(s) vote2013; coercing to match list name",
        fixed = TRUE
    )
    
    #colname
    expect_identical(
        Rakehelper:::setWeightTargetNames(weightTargetNames = c("vote2013","eastwest","gender"), targets = bad_listnames.w8margin, match.vars.by = "colname", isw8margin = c(TRUE,TRUE,TRUE)),
        targets_main.w8margin
    )
})



## ==== DEPRECATED ====
## ---- TARGET LEVELS MATCH OBSERVED LEVELS ----

# #surplus levels (empty) in observed
# #----with match.levels.by = name ----
# #surplus levels (empty) in observed
# # Expected - warning    Observed data contains empty factor level
# rakew8(no_unknowns_10cat.df, targets_known.w8margin)
# 
# #surplus levels (non-empty) in observed
# # Expected - error
# rakew8(gles17, targets_known.w8margin)
# 
# #surplus levels (empty) in target
# # Expected - warning   Empty target level(s) UNKNOWN do not match with any observed data on variable vote2013
# #rakew8(no_unknowns_9cat.df, targets_zero.w8margin)
# 
# #surplus levels (non-empty) in target
# # Expected - error
# rakew8(no_unknowns_9cat.df, targets_main.w8margin)
# 
# #non-matching level names (equal number of levels)
# # expected - error target does not match observed data (from rakew8)
# # plus warning   variable levels GREEN, LEFT, OTHER in target vote2013 are missing from observed factor variable
# rakew8(gles17, targets_en.w8margin)
# 
# #non-matching level names (more levels in target)
# # expected - error   Target does not match observed data on variable(s) vote2013
# rakew8(no_unknowns_9cat.df, targets_en.w8margin)
# 
# #non-matching level names (more levels in observed)
# rakew8(gles17, targets_en_known.w8margin)
#----with match.levels.by = order ----
# # expected - warning
# rakew8(no_unknowns_10cat.df, targets_known.w8margin, match.levels.by = "order")
# 
# #surplus levels (non-empty) in observed
# # expected - error
# rakew8(gles17, targets_known.w8margin, match.levels.by = "order")
# 
# #surplus levels (empty) in target
# # expected - error
# rakew8(no_unknowns_9cat.df, targets_zero.w8margin, match.levels.by = "order")
# 
# #surplus levels (non-empty) in target
# # expected - erorr
# rakew8(no_unknowns_9cat.df, targets_main.w8margin, match.levels.by = "order")
# 
# # non-matching level names (equal numbers of levels)
# # expected - pass
# 
# #non-matching level names (more levels in target)
# rakew8(no_unknowns_9cat.df, targets_en.w8margin, match.levels.by = "order")
# 
# rakew8(gles17, targets_en_known.w8margin, match.levels.by = "order")

# test_that("rakew8 correctly resolves clash between target column name and target list name", {
#     # Expected - warning message, coercing to match list name (from setWeightTargetNames)
#     expect_warning(
#         rakew8(gles17, targets = bad_colnames.w8margin), #Names of list are correct; name of column doesn't match any known variable
#         regexp = "w8margin column name(s) pastvote do not match list name(s) vote2013; coercing to match list name",
#         fixed = TRUE
#     )
#     
#     # Expected - warning message, coercing to match column name (from getWeightTargetNames)
#     expect_warning(
#         rakew8(gles17, targets = bad_listnames.w8margin, match.vars.by = "colname"), # Names of columns are correct; names of list  don't match any known variables
#         regexp = "w8margin column name(s) vote2013 do not match list name(s) past_vote; coercing to match column name",
#         fixed = TRUE
#     )
# })




    