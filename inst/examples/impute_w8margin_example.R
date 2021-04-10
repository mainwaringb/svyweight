turnout_w8margin <- as.w8margin(
    c(voted = .715, `did not vote` = .285, ineligible = NA), 
    varname = "turnout2013", 
    na.allow = TRUE,
    samplesize = 1500)
impute_w8margin(turnout_w8margin, observed = gles17$turnout2013)