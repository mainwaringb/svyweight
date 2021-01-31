library(Rakehelper)
library(testthat)

## ==== TEST AS_W8MARGIN ==== 

# ---- Vector/matrix ----
test_that("as.w8margin correctly converts vector and matrix targets", {
    # ---- Good behavior ----
    # Basic functionality
    expect_equal(
        as.w8margin(targets.vec$vote2013, varname = "vote2013")$Freq,
        c(.297, .184, .034, .060, .061, .034, .045, .185, .050, .050)
    )
    
    # Sample size functionality
    expect_equal(
        as.w8margin(targets.vec$vote2013, varname = "vote2013", samplesize = 1000)$Freq,
        c(297, 184, 034, 060, 061, 034, 045, 185, 50, 50)
    )
    
    # Rebase functionality
    expect_equal(
        expect_warning(
            sum(as.w8margin(targets.vec$vote2013[1:5], varname = "vote2013", samplesize = 1000)$Freq),
            "original targets for variable vote2013 sum to 0.636 and will be rebased"
        ),
        1000
    )
    
    # Matrix targets
    expect_equal(
        as.w8margin(targets.mat$gender_educ_valid, varname = "foo")$Freq,
        c(.15, .17, .17, .19, .16, .14)
    )
    
    # Specified levels functionality
    expect_equivalent(
        as.w8margin(as.numeric(targets.vec$vote2013), varname = "vote2013", levels = names(targets.vec$vote2013)),
        targets.df$vote2013
    )
    
    # ---- Error-catching ----
    
    # No levels specified
    expect_error(
        as.w8margin(c(.1,2,.4,9), varname = "foo"),
        "Vector has invalid or missing names; try specifying levels"
    )
    
    # Incorrect levels specified
    expect_error(
        as.w8margin(c(.1, 2, .4, 9), varname = "foo", levels = c("a", "b", "c")),
        "levels must be of length 4"
    )
    
})


# ---- Data frame targets ----
test_that("as.w8margin correctly converts data.frame targets", {
    
    # ---- Good behavior ----
    # Basic check - two columns
    expect_equivalent( # "equivalent" does not check attributes
        as.w8margin(targets.df$vote2013, varname = NULL),
        targets.df$vote2013
    )
    
    # Basic check - one column plus name
    expect_equivalent(
        as.w8margin(targets.df$vote2013_name_only, varname = NULL),
        targets.df$vote2013
    )
    
    # Check that column name is renamed correctly
    expect_equal(
        colnames(as.w8margin(targets.df$vote2013, varname = "foo")),
        c("foo", "Freq")
    )
    
    # Check that unusually-named input column is handled
    expect_equivalent(
        as.w8margin(targets.df$vote2013_wrong_name_freq, varname = NULL),
        targets.df$vote2013
    )
    
    # Check that columns are reordered for consistency
    expect_equal(
        colnames(as.w8margin(targets.df$vote2013_col_names_flipped, varname = NULL)),
        c("vote2013", "Freq")
    )
    
    # --- Error catching ----
    # Error on data frames of wrong size
    expect_error(
        as.w8margin(targets.df$vote2013_extra_col, varname = NULL),
        "Data frames must have one or two columns for conversion to w8margin"
    )
})


#----NA targets----

test_that("as.w8margin appropriately handles targets with NAs", {
    # expected error
    expect_error(
        as.w8margin(targets.vec$vote2013_na , varname = "vote2013", na.allow = FALSE),
        regexp = "Target is NA for level(s) INELIGIBLE, UNKNOWN, ",
        fixed = TRUE
    )
    
    expect_equal(
        as.w8margin(targets.vec$vote2013_na , varname = "vote2013", na.allow = TRUE)$Freq,
        c(.297, .184, .034, .060, .061, .034, .045, .285, NA, NA)
    )
})


## ===== TEST W8MARGIN_MATCHED ====

# --- Test core functionality ----
test_that("w8margin_matched correctly identifies non-matching targets", {
    #surplus levels in observed
    expect_warning(
        expect_false(w8margin_matched(targets_known.w8margin$vote2013, gles17$vote2013)),
        regexp = "Number of variable levels in observed data does not match length of target vote2013",
        fixed = TRUE
    )
    
    #surplus levels in target
    expect_warning(
        expect_false(w8margin_matched(targets_main.w8margin$vote2013, no_unknowns_9cat.df$vote2013)),
        regexp = "Number of variable levels in observed data does not match length of target vote2013",
        fixed = TRUE
    )
    
    #non-matching level names (more levels in observed)
    expect_warning(
        expect_false(w8margin_matched(targets_en_known.w8margin$vote2013, gles17$vote2013)),
        regexp = "Number of variable levels in observed data does not match length of target vote2013",
        fixed = TRUE
    )
    
    #non-matching level names (more levels in target)
    expect_warning(
        expect_false(w8margin_matched(targets_en.w8margin$vote2013, no_unknowns_9cat.df$vote2013)),
        regexp = "Number of variable levels in observed data does not match length of target vote2013",
        fixed = TRUE
    )
    
    #non-matching level names (equal number of levels)
    expect_warning(
        expect_false(w8margin_matched(targets_en_known.w8margin$vote2013, no_unknowns_9cat.df$vote2013)),
        regexp = "variable levels GREEN, LEFT, OTHER in target vote2013 are missing from observed factor variable",
        fixed = TRUE
    )
    expect_warning(
        expect_false(w8margin_matched(targets_en_known.w8margin$vote2013, no_unknowns_9cat.df$vote2013)),
        regexp = "variable levels GRUENE, DIE LINKE, andere Partei in observed factor variable are missing from target vote2013",
        fixed = TRUE
    )
    
    #factor levels are in same order, but rows of target are mixed up
    expect_true(w8margin_matched(targets_reorder.w8margin$eastwest, gles17_flipped_level.df$eastwest))
    
    # rows are in same order, but factor levels are mixed up
    expect_true(w8margin_matched(targets_main.w8margin$eastwest, gles17_flipped_level.df$eastwest))
    
    # everything is well-behaved
    expect_true(w8margin_matched(targets_main.w8margin$vote2013, gles17$vote2013))
})

# ---- Test unexpected input tpyes ----



## ===== TEST IMPUTE_W8MARGIN ====

test_that("impute_w8margin returns correctly imputed targets", {
    # Test with rebase = TRUE
    expect_equal(
        as.numeric(impute_w8margin(targets_na.w8margin$vote2013, observed = gles17$vote2013, rebase = TRUE)$Freq[9:10]),
        as.numeric((table(gles17$vote2013) / sum(table(gles17$vote2013)))[9:10])
    )
    
    # Test with rebase = FALSE
    expect_equal(
        impute_w8margin(all.w8margin$vote2013_na_count, observed = gles17$vote2013, rebase = FALSE)$Freq[1:8],
        all.w8margin$vote2013_na_count$Freq[1:8]
    )
    
    # Test with no NAS
    expect_equal(
        impute_w8margin(all.w8margin$vote2013, observed = gles17$vote2013),
        all.w8margin$vote2013
    )
    
    # Test with weights
    expect_equal(
        as.numeric(impute_w8margin(all.w8margin$vote2013_na, observed = gles17$vote2013, weights = gles17$dweight)$Freq[9:10]),
        as.numeric(svytable(~vote2013, design = svydesign(ids = gles17$vpoint, weights = gles17$dweight, 
                                               strata = gles17$eastwest, data = gles17, nest = TRUE), Ntotal = 1)[9:10])
    )
})
