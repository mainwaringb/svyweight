# Convert vector of percentages to w8margin
turnout2013_w8margin <- as.w8margin(
    c(voted = .715, `did not vote` = .285, ineligible = NA), 
    varname = "turnout2013", 
    na.allow = TRUE,
    samplesize = 1500)

# Convert matrix of percentages to w8margin
gender_educ_mat <- matrix(
    c(.15, .17, .17, .01, .19, .16, .14, .01),
    nrow = 2,
    byrow = TRUE, 
    dimnames = list(c("Male", "Female"), c("Low", "Medium", "High", NA)))
gender_educ_w8margin <- as.w8margin(gender_educ_mat, 
    varname = "gender_educ", samplesize = 2179)

# Convert data frame of counts to w8margin
# Keep default values for samplesize and varname
region_df <- data.frame(
    eastwest = c("east", "west"), Freq = c(425, 1754))
region_w8margin <- as.w8margin(region_df, 
                               levels = c("East Germany", "West Germany"), 
                               varname = NULL)

# Calculate rake weights using w8margin objects (without NAs)
require(survey)
gles17_dweighted <- svydesign(ids = gles17$vpoint, weights = gles17$dweight, 
    strata = gles17$eastwest, data = gles17, nest = TRUE)
rake(design = gles17_dweighted, 
     sample.margins = list(~gender_educ, ~eastwest),
     population.margins = list(gender_educ_w8margin, region_w8margin))


