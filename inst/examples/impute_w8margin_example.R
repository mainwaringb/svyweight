turnout_w8margin <- as.w8margin(
    c(voted = .715, `did not vote` = .285, ineligible = 0), 
    varname = "turnout2013", 
    samplesize = 1500)
turnout_w8margin[3,"Freq"] <- NA
impute_w8margin(turnout_w8margin, gles17$turnout2013)