# Convert vector of percentages to w8margin
age_vec <- c("<=29" = .16, "30-39" = .15, "40-49" = .15, 
                    "50-59" = .19, "60-69" = .15, ">=70" = .21)
age_w8margin <- as.w8margin(age_vec, varname = "agecat", samplesize = 2179)

# Convert matrix of percentages to w8margin
gender_educ_mat <- matrix(
    c(.15, .17, .17, .01, .19, .16, .14, .01),
    nrow = 2,
    byrow = TRUE, 
    dimnames = list(c("Male", "Female"), c("Low", "Medium", "High", NA)))
gender_educ_w8margin <- as.w8margin(educ_gender_mat, 
    varname = "gender_educ", samplesize = 2179)

# Calculate rake weights using w8margin objects
gles17_dweighted <- svydesign(ids = gles17$vpoint, weights = gles17$dweight, 
    strata = gles17$eastwest, data = gles17, nest = TRUE)
rake(design = gles17_dweighted, 
     sample.margins = list(~agecat, ~gender_educ),
     population.margins = list(age_w8margin, gender_educ_w8margin))

# Convert data frame of counts to w8margin
# Keep default values for samplesize and varname
region_df <- data.frame(
    eastwest = c("east", "west"), Freq = c(425, 1754))
region_w8margin <- as.w8margin(region_df, levels = c("East Germany", "West Germany"))
rake(design = gles17_dweighted,
     sample.margins = list(~agecat, ~eastwest),
     population.margins = list(age_w8margin, region_w8margin))