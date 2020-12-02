library(foreign)
library(Rakehelper)
library(testthat)

## ==== To Do ====
# Replace testing on 0% targets with direct testing of dropZeroTargets function
# Test sample size and rebase.tol parameters

## ==== Set up example data (2017 German Election Study) ====

# ---- Note on key variables ----
#bula - state
#ostwest - state location
#q1 - gender
#q2a - birth year
#q2b - birth month
#q2d - eligible to vote
#q11ba - vote intent
#q36 - eligible in 2013
#37 - voted in 2013
#q38ba - 2013 vote

# ---- Define datasets and variables ----
# Read 2017 German election study (pre-election wave)
de2017.df <- suppressWarnings(read.spss("Data/ZA6800_v5-0-0.sav", to.data.frame = TRUE))

#Recode variables for weighting
de2017.df$vote2013 <- factor(de2017.df$q38ba, levels = c(
    "CDU/CSU", 
    "SPD", 
    "FDP",
    "GRUENE", 
    "DIE LINKE", 
    "AfD", 
    "andere Partei", 
    "ABSTAIN",
    "INELIGIBLE",
    "UNKNOWN"))
de2017.df$vote2013[is.na(de2017.df$q36) | de2017.df$q36 == "nein"] <- "INELIGIBLE"
de2017.df$vote2013[!is.na(de2017.df$q37) & de2017.df$q37 == "nein, habe nicht gewaehlt"] <- "ABSTAIN"
de2017.df$vote2013[is.na(de2017.df$vote2013)] <- "UNKNOWN"
table(de2017.df$vote2013)

# Flip order of levels for one variable
de2017_flipped_level.df <- de2017.df
de2017_flipped_level.df$ostwest <- factor(de2017.df$ostwest, levels = c("Westdeutschland", "Ostdeutschland"))

# Create a variable with one extra level
# de2017_ostwest_3cat.df <- de2017.df
# de2017_ostwest_3cat.df$ostwest <- factor(de2017_ostwest_3cat.df$ostwest, levels = c("Ostdeutschland", "Westdeutschland", "Berlin"))
# de2017_ostwest_3cat.df$ostwest[1:50] <- "Berlin"
# 
# Subset to drop all cases of one level
no_unknowns_10cat.df <- de2017.df[de2017.df$vote2013 != "UNKNOWN",] # keep level in definition of factor
no_unknowns_9cat.df  <- no_unknowns_10cat.df
no_unknowns_9cat.df$vote2013 <- factor(no_unknowns_9cat.df$vote2013)

implicit_na.df <- de2017.df
implicit_na.df$ostwest[1:50] <- NA
explicit_na.df <- implicit_na.df
explicit_na.df$ostwest <- addNA(implicit_na.df$ostwest)

# Create svydesign object
de2017.svy <- suppressWarnings(svydesign(~1, data = de2017.df))

# Bad dataframe where one level consists only of cases with zero design weights
zero_dweights <- weights(de2017.svy)
zero_dweights[de2017.svy$variables$vote2013 == "INELIGIBLE"] <- 0
de2017_zero_dweight.svy <- svydesign(~1, weights = zero_dweights, data = de2017.df)

# Bad dataframe where one levels consists only of cases that will be dropped due to a 0% target on another variable
de2017_bad_level.df <- de2017.df
de2017_bad_level.df$ostwest <- "Westdeutschland"
de2017_bad_level.df$ostwest[de2017.df$vote2013 == "UNKNOWN"] <- "Ostdeutschland"
de2017_bad_level.df$ostwest <- factor(de2017_bad_level.df$ostwest, levels = levels(de2017.df$ostwest))


# ---- Define main targets ----
#Define targets (note that these targets may not be accurate - they are examples only)
targets.vec <- list(
    vote2013 = c(
        "CDU/CSU" = .297, 
        "SPD" = .184, 
        "FDP" = .034, 
        "GRUENE" = .060,
        "DIE LINKE" = .061, 
        "AfD" = .034, 
        "andere Partei" = .045, 
        "ABSTAIN" = .185, 
        "INELIGIBLE" = .050, 
        "UNKNOWN" = .050),
    ostwest = c(
        "Ostdeutschland" = .195, 
        "Westdeutschland" = .805),
    gender = c(
        "maennlich" = .495, 
        "weiblich" = .505)
)

# ---- define alternate targets  ----
# Targets with flipped level order
targets.vec$ostwest_reorder <- c(targets.vec$ostwest[2], targets.vec$ostwest[1])

# Targets with valid zeroes
targets.vec$vote2013_zero <- targets.vec$vote2013
targets.vec$vote2013_zero["INELIGIBLE"] <-.040
targets.vec$vote2013_zero["UNKNOWN"] <- 0
targets.vec$vote2013_zero["ABSTAIN"] <- .245

# Targets with NA *values*
targets.vec$vote2013_na <- targets.vec$vote2013
targets.vec$vote2013_na["INELIGIBLE"] <- NA
targets.vec$vote2013_na["UNKNOWN"] <- NA
targets.vec$vote2013_na["ABSTAIN"] <- .285

# Targets entirely omitting one level
targets.vec$vote2013_known <- targets.vec$vote2013[names(targets.vec$vote2013) != "UNKNOWN"]
targets.vec$vote2013_known["ABSTAIN"] <- .245
targets.vec$vote2013_known["INELIGIBLE"] <- .040

# Targets with level names altered (using English-language names for parties)
targets.vec$vote2013_en <- c(
    "CDU/CSU" = .297, 
    "SPD" = .184, 
    "FDP" = .034, 
    "GREEN" = .060, 
    "LEFT" = .061, 
    "AfD" = .034, 
    "OTHER" = .045, 
    "ABSTAIN" = .185, 
    "INELIGIBLE" = .050, 
    "UNKNOWN" = .050)
targets.vec$vote2013_en_known <- targets.vec$vote2013_en[names(targets.vec$vote2013_en) != "UNKNOWN"]
targets.vec$vote2013_en_known["ABSTAIN"] <- .245
targets.vec$vote2013_en_known["INELIGIBLE"] <- .040

# intentionally creating a bad target, with an unused level with no cases
targets.vec$vote2013_zero_bad <- targets.vec$vote2013_zero
targets.vec$vote2013_zero_bad["ASDF"] <- 0

# Creating a target with a new level named NA
# Note that this is hashed out, bc it won't convert to as.w8margin
# targets.vec$ostwest_implicit_na <- targets.vec$ostwest
# targets.vec$ostwest_implicit_na <- c(targets.vec$ostwest, .01)
# targets.vec$ostwest_implicit_na["Westdeutschland"] <- .795
# names(targets.vec$ostwest_implicit_na)[3] <- NA

## ==== CREATE W8MARGIN OBJECTS ====
# ---- main w8margin object ----
targets.w8margin <- list(
    vote2013 = as.w8margin(targets.vec$vote2013, varname = "vote2013"),
    ostwest = as.w8margin(targets.vec$ostwest, varname = "ostwest"),
    q1 = as.w8margin(targets.vec$gender, varname = "q1")
)

# ---- Modified but useful w8margin objects 0000
# Target for a quota of zero on some variable categories
targets_zero.w8margin <- list(
    vote2013 = as.w8margin(targets.vec$vote2013_zero, varname = "vote2013"),
    ostwest = as.w8margin(targets.vec$ostwest, varname = "ostwest"),
    q1 = as.w8margin(targets.vec$gender, varname = "q1")
)

# Target omitting some variable categories
targets_known.w8margin <- targets.w8margin
targets_known.w8margin$vote2013 <- as.w8margin(targets.vec$vote2013_known, varname = "vote2013")

# Targets changing level names (using English-language translations of party names)
targets_en.w8margin <- list(
    vote2013 = as.w8margin(targets.vec$vote2013_en, varname = "vote2013"),
    ostwest = as.w8margin(targets.vec$ostwest, varname = "ostwest"),
    q1 = as.w8margin(targets.vec$gender, varname = "q1")
)
targets_en_known.w8margin <- list(
    vote2013 = as.w8margin(targets.vec$vote2013_en_known, varname = "vote2013"),
    ostwest = as.w8margin(targets.vec$ostwest, varname = "ostwest"),
    q1 = as.w8margin(targets.vec$gender, varname = "q1")
)

# Targets changing order of levels
targets_reorder.w8margin <- list( #match.levels.by = name
    vote2013 = as.w8margin(targets.vec$vote2013, varname = "vote2013"), 
    ostwest = as.w8margin(targets.vec$ostwest_reorder, varname = "ostwest"), 
    q1 = as.w8margin(targets.vec$gender, varname = "q1")
)

# ---- define intentionally problematic targets ----
bad_colnames.w8margin <- targets.w8margin
names(bad_colnames.w8margin$vote2013) <- c("pastvote", "Freq")

bad_zero_level.w8margin <- list(
    vote2013 = as.w8margin(targets.vec$vote2013_zero_bad , varname = "vote2013"),
    ostwest = as.w8margin(targets.vec$ostwest, varname = "ostwest"),
    q1 = as.w8margin(targets.vec$gender, varname = "q1")
)

bad_listnames.w8margin <- list(
    past_vote = targets.w8margin$vote2013, 
    ostwest = targets.w8margin$ostwest,
    q1 = targets.w8margin$q1)


# ---- define targets with NA *level* ----
implicit_zero_target.w8margin <- targets.w8margin
implicit_zero_target.w8margin$ostwest <- rbind(implicit_zero.w8margin$ostwest, c(NA, .01))
implicit_zero_target.w8margin$ostwest$Freq <- c(.185, .785, .030)

explicit_zero_target.w8margin <- implicit_zero_target.w8margin
explicit_zero_target.w8margin$ostwest$ostwest <- addNA(explicit_zero_target.w8margin$ostwest$ostwest)


## ==== DEFINE BENCHMARKS FOR TESTING AGAINST ====

# Should eventually be replaced with
# A) manual vector of results, AND
# B) results generated from underlying rake call
benchmark_out <- rakew8(de2017.df,
                   targets = targets.w8margin)

# Create cannonical vector for one variable
benchmark_onevar_out <- rakew8(de2017.df, targets = list(vote2013 = targets.w8margin$vote2013), match.vars.by = "listname")


## ==== CHECK BASIC FUNCTIONALITY ====

# ---- Central case for tests, importing targets already in w8margin form ----
# Ensure that results haven't changed over time

test_that("rakew8 expected weights are generated using basic common parameters", {
    expect_identical( 
        rakew8(de2017.df,
               targets = targets.w8margin,
               match.vars.by = "listname",
               match.levels.by = "name"), 
        benchmark_out
    )
    
    expect_identical(
        rakew8(de2017.df, 
               targets = targets.w8margin, 
               match.vars.by = "colname",
               match.levels.by = "name"), 
        benchmark_out
    )
    
    expect_identical( 
        rakew8(de2017.df,
               targets = targets.w8margin,
               match.vars.by = "listname",
               match.levels.by = "order"), 
        benchmark_out
    )
    
    expect_identical(
        rakew8(de2017.df, 
               targets = targets.w8margin, 
               match.vars.by = "colname",
               match.levels.by = "order"), 
        benchmark_out
    )
})

# ---- Check that default parameters work as expected ----
test_that("rakew8 default parameters behave as expected", {
    expect_identical( # Should generate an error if, for some reason, we are defaulting to match.vars.by = "varname"
        rakew8(de2017.df, 
               targets = bad_colnames.w8margin),
        rakew8(de2017.df, 
               targets = bad_colnames.w8margin,
               match.vars.by = "listname")
    )
    
    expect_identical( #Should generate incorrect/unmatched results if for some reason defaulting to match.levels.by = "order"
        rakew8(de2017.df,
               targets = targets_reorder.w8margin
        ),
        rakew8(de2017.df,
               targets = targets_reorder.w8margin,
               match.levels.by = "name")
    )
    
    expect_identical( #Targets have sample size of 1.0, so if default goes to "from.targets" the weights won't match
        rakew8(de2017.df,
               targets = targets_reorder.w8margin
        ),
        rakew8(de2017.df,
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
    expect_identical(
        rakew8(de2017.df, 
               targets = list(
                   vote2013 = targets.vec$vote2013, 
                   ostwest = targets.vec$ostwest_reorder, 
                   q1 = targets.vec$gender),
               samplesize = "from.data"
               ),
        rakew8(de2017.df,
               targets = targets_reorder.w8margin,
               samplesize = "from.data")
    )
    
    expect_identical(
        rakew8(de2017.df, 
               targets = list(
                   vote2013 = targets.vec$vote2013, 
                   ostwest = targets.vec$ostwest_reorder, 
                   q1 = targets.vec$gender),
               samplesize = 1000
        ),
        rakew8(de2017.df,
               targets = targets_reorder.w8margin,
               samplesize = 1000)
    )
    
    expect_identical(
        rakew8(de2017.df, 
               targets = list(
                   vote2013 = targets.vec$vote2013, 
                   ostwest = targets.vec$ostwest_reorder, 
                   q1 = targets.vec$gender),
               samplesize = "from.targets" # these targets sum to 1.0, so we wouldn't really use them for sample size data
        ),
        rakew8(de2017.df,
               targets = targets_reorder.w8margin,
               samplesize = "from.targets")
    )
    
})


## ==== UNUSUAL TARGETS ====

#----only one target variable ----
# We want to test all these slightly different formulations, to ensure that lists arent getting dropped to vector
test_that("rakew8 correctly handles calls with only one weighting variable", {
    # Named list of 1 w8margin object - Expected pass
    expect_identical(
        rakew8(de2017.df, targets = list(vote2013 = targets.w8margin$vote2013), match.vars.by = "listname"),
        benchmark_onevar_out
    )
    expect_identical(
        rakew8(de2017.df, targets = list(targets.w8margin$vote2013), match.vars.by = "colname"),
        benchmark_onevar_out
    )
    
    # list of 1 vector - pass only if vector is named
    expect_identical( #Expected pass (named vector input)
        rakew8(de2017.df, targets = list(vote2013 = targets.vec$vote2013), match.vars.by = "listname"), # Named vector
        benchmark_onevar_out
    )
    expect_error( #Expected error (unnamed vector input)
        rakew8(de2017.df, targets = list(targets.vec$vote2013), match.vars.by = "listname"), 
        regexp = "List of weight targets must be named unless match.vars.by is set to 'colnames'",
        fixed = TRUE
    )
    
    # single vector, Expected error (make sure that a vector doesn't accidentally get accepted)
    expect_error(
        rakew8(de2017.df, targets = targets.vec$vote2013, match.vars.by = "listname"),
        "List of weight targets must be named unless match.vars.by is set to 'colnames'",
        fixed = TRUE
    )
    expect_error(
        rakew8(de2017.df, targets = targets.vec$vote2013, match.vars.by = "colname"),
        "match.vars.by = 'colname' requires targets of class w8margin",
        fixed = TRUE
    )
})

#----targets of 0% ----
# CONSIDER REPLACING THIS WITH UNIT TESTS ON dropZeroTargets function
test_that("rakew8 correctly handles target levels with a target of zero", {
    # Check that zero weights are included in dataset, rather than dropped
    # IE, the length of the output vector should be the same as nrow of the input data frame
    expect_length(
        rakew8(de2017.df, targets = targets_zero.w8margin), 
        nrow(de2017.df)
    )
    
    # Check that nonzero weights are correct
        # passing a long data frame with zero targets to rakew8 should produce the same nonzero weights 
        # as passing a shorter data frame, with zero target already removed
    expect_true( 
        all(
            rakew8(no_unknowns_9cat.df, targets_known.w8margin, samplesize = nrow(de2017.df)) # long data frame (more rows), with targets telling rakew8 to drop unwanted rows
            %in% 
            rakew8(de2017.df, targets = targets_zero.w8margin, samplesize = nrow(de2017.df)) # shorter data frame (fewer rows), with unwanted rows already dropped
        )
    )

    # Check that a zero target on invalid levels gives a warning
    expect_identical(
        expect_warning(
            rakew8(de2017.df, targets = bad_zero_level.w8margin),
            "Empty target level(s) ASDF do not match with any observed data on variable vote2013",
            fixed = TRUE
        ),
        rakew8(de2017.df, targets = targets_zero.w8margin)
    )
})


## ==== UNUSUAL OBSERVED VARIABLES ====

# ---- Observed variable levels with zero cases ----
test_that("rakew8 handles observed data with empty levels", {
    #CASE 1: OBSERVED DATA LEVEL WITH ZERO CASES, HAS (NON-ZERO) TARGET: error
    expect_warning(
        expect_error(
            rakew8(no_unknowns_10cat.df, targets.w8margin),
            regexp = "Target does not match observed data on variable(s) vote2013",
            fixed = TRUE
        ),
        regexp = "Observed data for vote2013 contains empty factor level UNKNOWN",
        fixed = TRUE
    )
    
    #CASE 2: OBSERVED DATA LEVEL WITH ZERO CASES, ZERO TARGET
    # Pass
    expect_identical(
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
            rakew8(implicit_na.df, targets.w8margin),
            regexp = "Target does not match observed data on variable(s) ostwest",
            fixed =  TRUE
        ),
        regexp = "NAs in observed data for ostwest",
        fixed = TRUE
    )
    
    # NA in data (with NA factor level), no NA target
    expect_warning(
        expect_error(
            rakew8(explicit_na.df, targets.w8margin),
            regexp = "Target does not match observed data on variable(s) ostwest",
            fixed =  TRUE
        ),
        regexp = "Number of variable levels in observed data does not match length of target ostwest",
        fixed = TRUE
    )
    
    # NA in data (without NA factor level), implicit NA in target
    expect_warning(
        expect_error(
            rakew8(implicit_na.df, implicit_zero_target.w8margin),
            regexp = "Target does not match observed data on variable(s) ostwest",
            fixed = TRUE
        ),
        regexp = "NAs in observed data for ostwest",
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
            regexp = "Target does not match observed data on variable(s) ostwest",
            fixed = TRUE
        ),
        regexp = "NAs in observed data for ostwest",
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


## ==== FRINGE LEVELS ====

test_that("rakew8 generates appropriate errors when all cases in a level are dropped", {
    # One level has all zero design weights
    expect_warning(
        expect_error(
            rakew8(de2017_zero_dweight.svy, targets.w8margin),
            regexp = "Target does not match observed data on variable(s) vote2013",
            fixed = TRUE
        ),
        regexp = "All valid cases for vote2013 level(s) INELIGIBLE had weight zero and were dropped",
        fixed = TRUE
    )
    
    # One level is lost when another variable is dropped due to zero target 
    expect_warning(
        expect_error(
            rakew8(de2017_bad_level.df, targets_zero.w8margin),
            regexp = "Target does not match observed data on variable(s) ostwest",
            fixed = TRUE
        ),
        regexp = "All valid cases for ostwest level(s) Ostdeutschland had weight zero and were dropped",
        fixed = TRUE
    )
})

## ==== NEW "VARIABLES" PARAMETER ====
# This is another area where it is probably better to develop more direct unit tests
# IE, what are the stress scenarios where either:
#   A) the formula could contain something syntactically valid but unexpected (an interaction term? a right-hand side?)
#   B) the recoding isn't saved correctly, perhaps because of a name conflict with an existing variable
#   C) recoded variables aren't matched correctly with targets
# But let's finish fleshing out the core function first!

# ---- Basic use cases ----
# rakew8(de2017.df,
#         variables = c(
#             ~plyr::revalue(q1, c(maennlich = "Male", weiblich = "Female")),
#             ~plyr::revalue(vote2013, c(
#                 "CDU/CSU" = "EST", "SPD" = "EST", "FDP" = "EST", "GRUENE" = "EST",
#                 "DIE LINKE" = "NEW", "AfD" = "NEW", "andere Partei" = "NEW",
#                 "ABSTAIN" = "NONE", "INELIGIBLE" = "NONE", "UNKNOWN" = "NONE"
#             ))
#         ),
#         targets = list(
#             c(Male = .48, Female = .52),
#             c(EST = .6, NEW = .25, NONE = .15)
#          )
# )
# 
# rakew8(de2017.df,
#         variables = c(
#             ~plyr::revalue(q1, c(maennlich = "Male", weiblich = "Female")),
#             ~plyr::mapvalues(as.numeric(vote2013),
#                 from = 1:10,
#                 to = c(1,1,1,1,1,2,2,3,3,3)
#             )
#         ),
#         targets = list(
#             c(Male = .48, Female = .52),
#             c(EST = .6, NEW = .25, NONE = .15)
#         ),
#         match.levels.by = "order"
# )


## ==== HELPER FUNCTIONS ====

#----Targets where column names clash with list names----
# Consider replacing this with tests of getWeightTargetNames and setWeightTargetNames functions
# (targets, match.vars.by, isw8margin)
test_that("getWeightTargetNames correctly resolves clash between target column name and target list name", {
    # listname
    expect_identical(
        Rakehelper:::getWeightTargetNames(bad_colnames.w8margin, match.vars.by = "listname", isw8margin = c(TRUE,TRUE,TRUE)),
        c("vote2013", "ostwest", "q1")
    )
    
    # colname
    expect_warning(
        expect_identical(
            as.vector(Rakehelper:::getWeightTargetNames(bad_colnames.w8margin, match.vars.by = "colname", isw8margin = c(TRUE,TRUE,TRUE))),
            c("pastvote", "ostwest", "q1")
        ),
        regexp = "w8margin column name(s) pastvote do not match list name(s) vote2013; coercing to match column name",
        fixed = TRUE
    )
})

test_that("setWeightTargetNames correctly renames weight targets", {
    #listname
    expect_warning(
        expect_identical(
            Rakehelper:::setWeightTargetNames(weightTargetNames = c("vote2013", "ostwest", "q1"), targets = bad_colnames.w8margin, match.vars.by = "listname", isw8margin = c(TRUE,TRUE,TRUE)),
            targets.w8margin
        ),
        regexp = "w8margin column name(s) pastvote do not match list name(s) vote2013; coercing to match list name",
        fixed = TRUE
    )
    
    #colname
    expect_identical(
        Rakehelper:::setWeightTargetNames(weightTargetNames = c("vote2013","ostwest","q1"), targets = bad_listnames.w8margin, match.vars.by = "colname", isw8margin = c(TRUE,TRUE,TRUE)),
        targets.w8margin
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
        rakew8(de2017.df, targets = list(ostwest = targets.vec$ostwest, q1 = targets.vec$gender,
                                         vote2013 = targets.vec$vote2013_na)),
        regexp = "Target is NA for level(s) INELIGIBLE, UNKNOWN, ",
        fixed = TRUE
    )
})

# --- Targets that don't match with observed data are flagged ----
test_that("w8margin.matched correctly identifies non-matching targets", {
    #surplus levels in observed
    expect_warning(
        expect_false(w8margin.matched(targets_known.w8margin$vote2013, de2017.df$vote2013)),
        regexp = "Number of variable levels in observed data does not match length of target vote2013",
        fixed = TRUE
    )
    
    #surplus levels in target
    expect_warning(
        expect_false(w8margin.matched(targets.w8margin$vote2013, no_unknowns_9cat.df$vote2013)),
        regexp = "Number of variable levels in observed data does not match length of target vote2013",
        fixed = TRUE
    )
    
    #non-matching level names (more levels in observed)
    expect_warning(
        expect_false(w8margin.matched(targets_en_known.w8margin$vote2013, de2017.df$vote2013)),
        regexp = "Number of variable levels in observed data does not match length of target vote2013",
        fixed = TRUE
    )
    
    #non-matching level names (more levels in target)
    expect_warning(
        expect_false(w8margin.matched(targets_en.w8margin$vote2013, no_unknowns_9cat.df$vote2013)),
        regexp = "Number of variable levels in observed data does not match length of target vote2013",
        fixed = TRUE
    )
    
    #non-matching level names (equal number of levels)
    expect_warning(
        expect_false(w8margin.matched(targets_en_known.w8margin$vote2013, no_unknowns_9cat.df$vote2013)),
        regexp = "variable levels GREEN, LEFT, OTHER in target vote2013 are missing from observed factor variable",
        fixed = TRUE
    )
    expect_warning(
        expect_false(w8margin.matched(targets_en_known.w8margin$vote2013, no_unknowns_9cat.df$vote2013)),
        regexp = "variable levels GRUENE, DIE LINKE, andere Partei in observed factor variable are missing from target vote2013",
        fixed = TRUE
    )
    
    #factor levels are in same order, but rows of target are mixed up
    expect_true(w8margin.matched(targets_reorder.w8margin$ostwest, de2017_flipped_level.df$ostwest))
    
    # rows are in same order, but factor levels are mixed up
    expect_true(w8margin.matched(targets.w8margin$ostwest, de2017_flipped_level.df$ostwest))
    
    # everything is well-behaved
    expect_true(w8margin.matched(targets.w8margin$vote2013, de2017.df$vote2013))
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
# rakew8(de2017.df, targets_known.w8margin)
# 
# #surplus levels (empty) in target
# # Expected - warning   Empty target level(s) UNKNOWN do not match with any observed data on variable vote2013
# #rakew8(no_unknowns_9cat.df, targets_zero.w8margin)
# 
# #surplus levels (non-empty) in target
# # Expected - error
# rakew8(no_unknowns_9cat.df, targets.w8margin)
# 
# #non-matching level names (equal number of levels)
# # expected - error target does not match observed data (from rakew8)
# # plus warning   variable levels GREEN, LEFT, OTHER in target vote2013 are missing from observed factor variable
# rakew8(de2017.df, targets_en.w8margin)
# 
# #non-matching level names (more levels in target)
# # expected - error   Target does not match observed data on variable(s) vote2013
# rakew8(no_unknowns_9cat.df, targets_en.w8margin)
# 
# #non-matching level names (more levels in observed)
# rakew8(de2017.df, targets_en_known.w8margin)
#----with match.levels.by = order ----
# # expected - warning
# rakew8(no_unknowns_10cat.df, targets_known.w8margin, match.levels.by = "order")
# 
# #surplus levels (non-empty) in observed
# # expected - error
# rakew8(de2017.df, targets_known.w8margin, match.levels.by = "order")
# 
# #surplus levels (empty) in target
# # expected - error
# rakew8(no_unknowns_9cat.df, targets_zero.w8margin, match.levels.by = "order")
# 
# #surplus levels (non-empty) in target
# # expected - erorr
# rakew8(no_unknowns_9cat.df, targets.w8margin, match.levels.by = "order")
# 
# # non-matching level names (equal numbers of levels)
# # expected - pass
# 
# #non-matching level names (more levels in target)
# rakew8(no_unknowns_9cat.df, targets_en.w8margin, match.levels.by = "order")
# 
# rakew8(de2017.df, targets_en_known.w8margin, match.levels.by = "order")

# test_that("rakew8 correctly resolves clash between target column name and target list name", {
#     # Expected - warning message, coercing to match list name (from setWeightTargetNames)
#     expect_warning(
#         rakew8(de2017.df, targets = bad_colnames.w8margin), #Names of list are correct; name of column doesn't match any known variable
#         regexp = "w8margin column name(s) pastvote do not match list name(s) vote2013; coercing to match list name",
#         fixed = TRUE
#     )
#     
#     # Expected - warning message, coercing to match column name (from getWeightTargetNames)
#     expect_warning(
#         rakew8(de2017.df, targets = bad_listnames.w8margin, match.vars.by = "colname"), # Names of columns are correct; names of list  don't match any known variables
#         regexp = "w8margin column name(s) vote2013 do not match list name(s) past_vote; coercing to match column name",
#         fixed = TRUE
#     )
# })


    