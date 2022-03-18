# Computing only rake weights
# EG, for a survey conducted with simple random sampling
gles17$simple_weight <- rakew8(design = gles17, 
    gender ~ c("Male" = .495, "Female" = .505),
    eastwest ~ c("East Germany" = .195, "West Germany" = .805)
)

# Specifying a recode of variable in observed dataset
require(dplyr)
gles17_raked <- rakesvy(design = gles17, 
    gender ~ c("Male" = .495, "Female" = .505),
    dplyr::recode(agecat, `<=29` = "<=39", `30-39` = "<=39") ~ 
        c("<=39" = .31, "40-49" = .15, "50-59" = .19, "60-69" = .15, ">=70" = .21)
)

# Computing rake weights after design weights
# EG, for a survey with complex sampling design
require(survey)
gles17_dweighted <- svydesign(ids = gles17$vpoint, weights = gles17$dweight, 
    strata = gles17$eastwest, data = gles17, nest = TRUE)
gles17_raked <- rakesvy(design = gles17_dweighted, 
    gender ~ c("Male" = .495, "Female" = .505),
    agecat ~ c("<=29" = .16, "30-39" = .15, "40-49" = .15, 
        "50-59" = .19, "60-69" = .15, ">=70" = .21)
)

# With unnamed target levels, using match.levels.by = "order"
rakew8(design = gles17, 
    gender ~ c(.495, .505),
    eastwest ~ c(.195, .805),
    match.levels.by = "order"
)
