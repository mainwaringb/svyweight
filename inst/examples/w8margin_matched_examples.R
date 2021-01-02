gender_w8margin <- as.w8margin(
    c(Male = .49, Female = .51),
    varname = "gender", 
    samplesize = 2179)
# Returns TRUE
w8margin_matched(gender_w8margin, gles17$gender)

gender_w8margin_alt <- as.w8margin(
    c(man = .49, woman = .51), 
    varname = "gender", 
    samplesize = 2179)
# Returns FALSE - level names in gles17$gender do not match level names in gender_w8margin_alt
w8margin_matched(gender_w8margin_alt, gles17$gender)

agecat_50plus_w8margin <- as.w8margin(
    c("50-59" = .35, "60-69" = .27, ">=70" = .38),
    varname = "educ",
    samplesize = 2179
)
gles17_50plus <- gles17[gles17$agecat %in% c("50-59", "60-69", ">=70"),]
# Returns FALSE - gles17$agecat has empty factor levels for <=29, 30-39, 40-49
w8margin_matched(agecat_50plus_w8margin, gles17_50plus$agecat)
# Returns TRUE - gles17$agecat is refactored to drop empty levels
w8margin_matched(agecat_50plus_w8margin, gles17_50plus$agecat, refactor = TRUE)
