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
               vote2013 ~ targets_main.w8margin$vote2013, 
               eastwest ~ targets_main.w8margin$eastwest, 
               gender ~ targets_main.w8margin$gender,
               match.vars.by = "object.name",
               match.levels.by = "name"), 
        Rakehelper:::benchmark_out
    )
    
    expect_equal(
        rakew8(gles17, 
               vote2013 ~ targets_main.w8margin$vote2013, 
               eastwest ~ targets_main.w8margin$eastwest, 
               gender ~ targets_main.w8margin$gender, 
               match.vars.by = "col.name",
               match.levels.by = "name"), 
        Rakehelper:::benchmark_out
    )
    
    expect_equal( 
        rakew8(gles17,
               vote2013 ~ targets_main.w8margin$vote2013, 
               eastwest ~ targets_main.w8margin$eastwest, 
               gender ~ targets_main.w8margin$gender,
               match.vars.by = "object.name",
               match.levels.by = "order"), 
        Rakehelper:::benchmark_out
    )
    
    expect_equal(
        rakew8(gles17, 
               vote2013 ~ targets_main.w8margin$vote2013, 
               eastwest ~ targets_main.w8margin$eastwest, 
               gender ~ targets_main.w8margin$gender, 
               match.vars.by = "col.name",
               match.levels.by = "order"), 
        Rakehelper:::benchmark_out
    )
})

# ---- Check that default parameters work as expected ----
test_that("rakew8 default parameters behave as expected", {
    expect_equal( # Should generate an error if, for some reason, we are defaulting to match.vars.by = "varname"
        suppressWarnings(rakew8(gles17, 
               vote2013 ~ bad_colnames.w8margin$vote2013,
               eastwest ~ bad_colnames.w8margin$eastwest,
               gender ~ bad_colnames.w8margin$gender)),
        suppressWarnings(rakew8(gles17, 
                vote2013 ~ bad_colnames.w8margin$vote2013,
                eastwest ~ bad_colnames.w8margin$eastwest,
                gender ~ bad_colnames.w8margin$gender,
                match.vars.by = "object.name"))
    )
    
    expect_equal( #Should generate incorrect/unmatched results if for some reason defaulting to match.levels.by = "order"
        rakew8(gles17,
               vote2013 ~ targets_reorder.w8margin$vote2013, 
               eastwest ~ targets_reorder.w8margin$eastwest, 
               gender ~ targets_reorder.w8margin$gender
        ),
        rakew8(gles17,
               vote2013 ~ targets_reorder.w8margin$vote2013, 
               eastwest ~ targets_reorder.w8margin$eastwest, 
               gender ~ targets_reorder.w8margin$gender,
               match.levels.by = "name")
    )
    
    expect_equal( #Targets have sample size of 1.0, so if default goes to "from.targets" the weights won't match
        rakew8(gles17,
               vote2013 ~ targets_reorder.w8margin$vote2013, 
               eastwest ~ targets_reorder.w8margin$eastwest, 
               gender ~ targets_reorder.w8margin$gender
        ),
        rakew8(gles17,
               vote2013 ~ targets_reorder.w8margin$vote2013, 
               eastwest ~ targets_reorder.w8margin$eastwest, 
               gender ~ targets_reorder.w8margin$gender,
               samplesize = "from.data")
    )
})

# ---- Check basic w8margin conversions ----
# Arguably we should develop more direct ways to test whether rakew8 calls to w8margin work as expected
# might involve creating a separate function for the "Convert targets to class w8margin" section and testing this
# plus tests on the generation of forcedTargetLevels
# (Also should develop separate unit testing for the w8margin function)

test_that("rakew8 converts vector target to w8margin objects correctly, with simple sample size settings", {
    expect_equal(
        rakew8(
            gles17, 
            vote2013 ~ targets.vec$vote2013, 
            eastwest ~ targets.vec$eastwest_reorder, 
            gender ~ targets.vec$gender,
            samplesize = "from.data"
        ),
        rakew8(
            gles17,
            vote2013 ~ targets_reorder.w8margin$vote2013, 
            eastwest ~ targets_reorder.w8margin$eastwest, 
            gender ~ targets_reorder.w8margin$gender,
            samplesize = "from.data"
        )
    )
    
    expect_equal(
        rakew8(
            gles17, 
            vote2013 ~ targets.vec$vote2013, 
            eastwest ~ targets.vec$eastwest_reorder, 
            gender ~ targets.vec$gender,
            samplesize = 1000
        ),
        rakew8(
            gles17,
            vote2013 ~ targets_reorder.w8margin$vote2013, 
            eastwest ~ targets_reorder.w8margin$eastwest, 
            gender ~ targets_reorder.w8margin$gender,
            samplesize = 1000
        )
    )
    
    expect_equal(
        rakew8(
            gles17, 
            vote2013 ~ targets.vec$vote2013, 
            eastwest ~ targets.vec$eastwest_reorder, 
            gender ~ targets.vec$gender,
            samplesize = "from.targets" # these targets sum to 1.0, so we wouldn't really use them for sample size data
        ),
        rakew8(
            gles17,
            vote2013 ~ targets_reorder.w8margin$vote2013, 
            eastwest ~ targets_reorder.w8margin$eastwest, 
            gender ~ targets_reorder.w8margin$gender,
            samplesize = "from.targets"
        )
    )
    
})


## ==== UNUSUAL TARGETS ====

#----only one target variable ----
# We want to test all these slightly different formulations, to ensure that lists arent getting dropped to vector
test_that("rakew8 correctly handles calls with only one weighting variable", {
    # Named list of 1 w8margin object - Expected pass
    expect_equal(
        rakew8(
            gles17, 
            vote2013 ~ targets_main.w8margin$vote2013, 
            match.vars.by = "object.name"),
        benchmark_onevar_out
    )
    expect_equal(
        rakew8(gles17, 
               ~targets_main.w8margin$vote2013, 
               match.vars.by = "col.name"),
        benchmark_onevar_out
    )
    
    
    # list of 1 vector - pass only if vector is named
    expect_equal( #Expected pass (named vector input)
        rakew8(gles17, vote2013 ~ targets.vec$vote2013, match.vars.by = "object.name"), # Named vector
        benchmark_onevar_out
    )
    expect_error( #Expected error (unnamed vector input)
        rakew8(gles17, ~ targets.vec$vote2013, match.vars.by = "object.name"), 
        regexp = "List of weight targets must be named unless match.vars.by is set to 'col.name'",
        fixed = TRUE
    )
    
    # single vector, Expected error (make sure that a vector doesn't accidentally get accepted)
    expect_error(
        rakew8(gles17, ~ targets.vec$vote2013, match.vars.by = "object.name"),
        "List of weight targets must be named unless match.vars.by is set to 'col.name'",
        fixed = TRUE
    )
    expect_error(
        rakew8(gles17, ~ targets.vec$vote2013, match.vars.by = "col.name"),
        "match.vars.by = 'col.name' requires targets of class w8margin",
        fixed = TRUE
    )
})
#----targets of 0% ----
# Check ultimate output from rakew8
test_that("rakew8 correctly handles target levels with a target of zero", {
    # Check that zero weights are included in dataset, rather than dropped
    # IE, the length of the output vector should be the same as nrow of the input data frame
    expect_length(
        rakew8(
            gles17, 
            vote2013 ~ targets_zero.w8margin$vote2013, 
            eastwest ~ targets_zero.w8margin$eastwest, 
            gender ~ targets_zero.w8margin$gender), 
        nrow(gles17)
    )
})
# See also DropZeroTargets helper function

# ---- Single list of targets ----
test_that("rakew8 corretly processes a single list of targets", {
    # Single list of data frames (more than one element)
    expect_equal(
        rakew8(
            gles17, 
            list(
                ~ targets_main.w8margin$vote2013,
                ~ targets_main.w8margin$eastwest,
                ~ targets_main.w8margin$gender
            ),
            samplesize = "from.data",
            match.vars.by = "col.name"
        ),
        rakew8(
            gles17, 
            vote2013 ~ targets_main.w8margin$vote2013, 
            eastwest ~ targets_main.w8margin$eastwest, 
            gender ~ targets_main.w8margin$gender,
            samplesize = "from.data",
            match.vars.by = "col.name"
        )
    )
    
    # Single list of vectors (more than one element)
    expect_equal(
        rakew8(
            gles17, 
            list(
               vote2013 ~ targets.vec$vote2013,
               eastwest ~ targets.vec$eastwest,
               gender ~ targets.vec$gender
            ),
            samplesize = "from.data"
        ),
        rakew8(
            gles17, 
            vote2013 ~ targets.vec$vote2013, 
            eastwest ~ targets_main.w8margin$eastwest, 
            gender ~ targets_main.w8margin$gender,
            samplesize = "from.data"
        )
    )
    
    # Single list of data frames (one element)
    # Important to separately test one element, bc a list of one element should be converted via list2env
    # whereas as a single vector should not!
    expect_equal(
        rakew8(
            gles17, 
            list(vote2013 ~ targets.vec$vote2013),
            samplesize = "from.data"
        ),
        rakew8(
            gles17, 
            vote2013 ~ targets.vec$vote2013, 
            samplesize = "from.data"
        )
    )

})

# ---- errors and warnings for fringe cases ----
test_that("rakew8 generates appropriate errors and warnings", {
    # Error when one level has all zero design weights
    expect_warning(
        expect_error(
            rakew8(
                gles17_zero_dweight.svy, 
                vote2013 ~ targets_main.w8margin$vote2013, 
                eastwest ~ targets_main.w8margin$eastwest, 
                gender ~ targets_main.w8margin$gender),
            regexp = "Target does not match observed data on variable(s) vote2013",
            fixed = TRUE
        ),
        regexp = "All valid cases for vote2013 level(s) INELIGIBLE had weight zero and were dropped",
        fixed = TRUE
    )
    
    # Error when one level is lost when another variable is dropped due to zero target 
    expect_warning(
        expect_error(
            rakew8(
                gles17_bad_level.df, 
                vote2013 ~ targets_zero.w8margin$vote2013, 
                eastwest ~ targets_zero.w8margin$eastwest, 
                gender ~ targets_zero.w8margin$gender),
            regexp = "Target does not match observed data on variable(s) eastwest",
            fixed = TRUE
        ),
        regexp = "All valid cases for eastwest level(s) East Germany had weight zero and were dropped",
        fixed = TRUE
    )
    
    # Error when zero target is specified on invalid level
    expect_warning(
        rakew8(
            gles17, 
            vote2013 ~ bad_zero_level.w8margin$vote2013, 
            eastwest ~ bad_zero_level.w8margin$eastwest, 
            gender ~ bad_zero_level.w8margin$gender),
        "Empty target level(s) ASDF do not match with any observed data on variable vote2013",
        fixed = TRUE
    )
    
})

# ---- NA in targets ----
# expected error
# error in as.w8margin.numeric
test_that("rakew8 appropriately handles NA targets", {
    expect_error(
        rakew8(gles17,
               eastwest ~ targets.vec$eastwest, 
               gender ~ targets.vec$gender,
               vote2013 ~ targets.vec$vote2013_na),
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
            rakew8(
                no_unknowns_10cat.df, 
                vote2013 ~ targets_main.w8margin$vote2013, 
                eastwest ~ targets_main.w8margin$eastwest, 
                gender ~ targets_main.w8margin$gender),
            regexp = "Target does not match observed data on variable(s) vote2013",
            fixed = TRUE
        ),
        regexp = "Observed data for vote2013 contains empty factor level UNKNOWN",
        fixed = TRUE
    )
    
    #CASE 2: OBSERVED DATA LEVEL WITH ZERO CASES, ZERO TARGET
    # Pass
    expect_equal(
        rakew8(
            no_unknowns_10cat.df, 
            vote2013 ~ targets_zero.w8margin$vote2013, 
            eastwest ~ targets_zero.w8margin$eastwest, 
            gender ~ targets_zero.w8margin$gender),
        rakew8(
            no_unknowns_9cat.df, 
            vote2013 ~ targets_known.w8margin$vote2013,
            eastwest ~ targets_zero.w8margin$eastwest,
            gender ~ targets_zero.w8margin$gender)
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
            rakew8(
                implicit_na.df, 
                vote2013 ~ targets_main.w8margin$vote2013, 
                eastwest ~ targets_main.w8margin$eastwest, 
                gender ~ targets_main.w8margin$gender),
            regexp = "Target does not match observed data on variable(s) eastwest",
            fixed =  TRUE
        ),
        regexp = "NAs in observed data for eastwest",
        fixed = TRUE
    )
    
    # NA in data (with NA factor level), no NA target
    expect_warning(
        expect_error(
            rakew8(
                explicit_na.df, 
                vote2013 ~ targets_main.w8margin$vote2013, 
                eastwest ~ targets_main.w8margin$eastwest, 
                gender ~ targets_main.w8margin$gender),
            regexp = "Target does not match observed data on variable(s) eastwest",
            fixed =  TRUE
        ),
        regexp = "Number of variable levels in observed data does not match length of target eastwest",
        fixed = TRUE
    )
    
    # NA in data (without NA factor level), implicit NA in target
    expect_warning(
        expect_error(
            rakew8(
                implicit_na.df, 
                vote2013 ~ implicit_zero_target.w8margin$vote2013, 
                eastwest ~ implicit_zero_target.w8margin$eastwest, 
                gender ~ implicit_zero_target.w8margin$gender),
            regexp = "Target does not match observed data on variable(s) eastwest",
            fixed = TRUE
        ),
        regexp = "NAs in observed data for eastwest",
        fixed = TRUE
    )
    
    # NA in data (with NA factor level), implicit NA in target
    # NOTE: UNINFORMATIVE ERROR MESSAGE HERE, SHOULD BE FIXED
    expect_error(
        rakew8(
            explicit_na.df, 
            vote2013 ~ implicit_zero_target.w8margin$vote2013, 
            eastwest ~ implicit_zero_target.w8margin$eastwest, 
            gender ~ implicit_zero_target.w8margin$gender),
        regexp = "Target does not match observed data on variable(s)",
        fixed = TRUE
    )
    
    # NA in data (without NA factor level), explicit NA in target
    expect_warning(
        expect_error(
            rakew8(
                implicit_na.df, 
                vote2013 ~ explicit_zero_target.w8margin$vote2013, 
                eastwest ~ explicit_zero_target.w8margin$eastwest, 
                gender ~ explicit_zero_target.w8margin$gender),
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
        rakew8(
            explicit_na.df, 
            vote2013 ~ explicit_zero_target.w8margin$vote2013, 
            eastwest ~ explicit_zero_target.w8margin$eastwest, 
            gender ~ explicit_zero_target.w8margin$gender),
        regexp = "Target does not match observed data on variable(s)",
        fixed = TRUE
    )
    
})

#---- samplesize and rebasetolerance NEED TESTS ----


## ==== HELPER FUNCTIONS ====

#----Targets where column names clash with list names (getWeightTargetNames/setWeightTargetNames) ----
test_that("getWeightTargetNames correctly resolves clash between target column name and target list name", {
    # object.name
    expect_identical(
        Rakehelper:::getWeightTargetNames(bad_colnames.w8margin, match.vars.by = "object.name", isw8margin = c(TRUE,TRUE,TRUE)),
        c("vote2013", "eastwest", "gender")
    )
    
    # col.name
    expect_warning(
        expect_identical(
            as.vector(Rakehelper:::getWeightTargetNames(bad_colnames.w8margin, match.vars.by = "col.name", isw8margin = c(TRUE,TRUE,TRUE))),
            c("pastvote", "eastwest", "gender")
        ),
        regexp = "target column name(s) pastvote do not match list name(s) vote2013; coercing to match column name",
        fixed = TRUE
    )
})

test_that("setWeightTargetNames correctly renames weight targets", {
    #object.name
    expect_warning(
        expect_identical(
            Rakehelper:::setWeightTargetNames(weightTargetNames = c("vote2013", "eastwest", "gender"), targets = bad_colnames.w8margin, match.vars.by = "object.name", isw8margin = c(TRUE,TRUE,TRUE)),
            targets_main.w8margin
        ),
        regexp = "w8margin column name(s) pastvote do not match list name(s) vote2013; coercing to match list name",
        fixed = TRUE
    )
    
    #col.name
    expect_identical(
        Rakehelper:::setWeightTargetNames(weightTargetNames = c("vote2013","eastwest","gender"), targets = bad_listnames.w8margin, match.vars.by = "col.name", isw8margin = c(TRUE,TRUE,TRUE)),
        targets_main.w8margin
    )
})

#----NA targets----
# The unit testing for these should be in the as.w8margin unit testing
# Until that is built out, keeping it here

test_that("rakew8 (via as.w8margin) appropriately handles targets with NA levels", {
    # expected error
    expect_error(
        as.w8margin(targets.vec$vote2013_na , varname = "vote2013"),
        regexp = "Target is NA for level(s) INELIGIBLE, UNKNOWN, ",
        fixed = TRUE
    )
    
    # expected error
    # error in as.w8margin.numeric
    expect_error(
        rakew8(
            gles17, 
            eastwest ~ targets.vec$eastwest, 
            gender ~ targets.vec$gender,
            vote2013 ~ targets.vec$vote2013_na),
        regexp = "Target is NA for level(s) INELIGIBLE, UNKNOWN, ",
        fixed = TRUE
    )
})


# ---- Checking zero targets (dropZeroTargets) ---- 
test_that("dropZeroTargets is dropping correct cases and refactoring", {
    # Dropping based on zero design weights - check if any weights are nonzero after dropping
    # Here, all cases with vote2013 == INELIGIBLE had design weights of zero and were dropped
    expect_false(
        expect_warning(
            any(
                weights(
                    Rakehelper:::dropZeroTargets(
                        gles17_zero_dweight.svy, zeroTargetLevels = list(vote2013 = c(), eastwest = c(), gender = c())))
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

# ---- Parsing weight formulas (parseWeightFormulas/extractTargets) ----
test_that("parseWeightFormulas computes appropriate transformations", {
    # Data is returned correctly
    expect_equal(
        parseTargetFormulas(
            target_formulas = list(
                dplyr::recode(agecat, `<=29` = "<=39", `30-39` = "<=39") ~ age_recode_vec,
                eastwest ~ c(`East Germany` = .805, `West Germany` = .195),
                ~ targets_main.w8margin$gender),
            design = gles17.svy
        ),
        list(
            data = data.frame(
                `dplyr.recode.agecat.29.39.30.39.39.` = dplyr::recode(gles17$agecat, `<=29` = "<=39", `30-39` = "<=39"),
                eastwest = gles17$eastwest
            ),
            varnames = list("dplyr.recode.agecat.29.39.30.39.39.", "eastwest", NULL)
        )
    )
    
    # Problematic formula - too many rows/columns
    expect_error(
        parseTargetFormulas(
            target_formulas = list(
                dplyr::recode(agecat, `<=29` = "<=39", `30-39` = "<=39") + eastwest ~ age_recode_vec,
                eastwest ~ c(`East Germany` = .805, `West Germany` = .195)),
            design = gles17.svy
        ),
        'Weight target formulas dplyr::recode(agecat, `<=29` = "<=39", `30-39` = "<=39") + eastwest ~ age_recode_vec do not produce 1 column of target data',
        fixed = TRUE
    )
    
    # Only one target (non-null name)
    expect_equal(
        parseTargetFormulas(
            target_formulas = list(
                dplyr::recode(agecat, `<=29` = "<=39", `30-39` = "<=39") ~ age_recode_vec),
            design = gles17.svy
        ),
        list(
            data = data.frame(
                `dplyr.recode.agecat.29.39.30.39.39.` = dplyr::recode(gles17$agecat, `<=29` = "<=39", `30-39` = "<=39")
            ),
            varnames = "dplyr.recode.agecat.29.39.30.39.39."
        )
    )
    
    # Only one target (null name)
    expect_equal(
        parseTargetFormulas(
            target_formulas = list(
                ~ targets_main.w8margin$vote2013),
            design = gles17.svy
        ),
        list(
            data = NULL,
            varnames = list(NULL)
        )
    )
    
})

test_that("extractTargets returns correct weight target object", {
    # Get targets both with and without LHS
    expect_equal(
        extractTargets(list(
            gender ~ targets_main.w8margin$vote2013, 
            ~ targets_main.w8margin$eastwest)),
        list(
            targets_main.w8margin$vote2013,
            targets_main.w8margin$eastwest
        )
    )
    
    # Try to evaluate missing targets
    expect_error(
        extractTargets(gender ~ targets_main.w8margin$vote2013asdf),
        "Right-hand side of target(s) gender ~ targets_main.w8margin$vote2013asdf is NULL or could not be found in specified environments",
        fixed = TRUE
    )
})
    