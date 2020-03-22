library(foreign)
source("Weight handling functions.R")

## ==== Set up example data (2017 German Election Study) ====

#Read 2017 German election study (pre-election wave)
de2017.df <- read.spss("Data/ZA6800_v5-0-0.sav", to.data.frame = TRUE)

#Define targets (note that these targets may not be accurate - they are examples only)
targets.vec <- list(
    vote2013 = c("CDU/CSU" = .297, "SPD" = .184, "FDP" = .034, "GRUENE" = .060, "DIE LINKE" = .061, "AfD" = .034, "andere Partei" = .045, "ABSTAIN" = .185, "INELIGIBLE" = .050, "UNKNOWN" = .050),
    ostwest = c("Ostdeutschland" = .195, "Westdeutschland" = .805),
    gender = c("maennlich" = .495, "weiblich" = .505)
)

#Recode variables for weighting
de2017.df$vote2013 <- factor(de2017.df$q38ba, levels = c("CDU/CSU", "SPD", "FDP", "GRUENE", "DIE LINKE", "AfD", "andere Partei", "ABSTAIN", "INELIGIBLE", "UNKNOWN"))
de2017.df$vote2013[is.na(de2017.df$q36) | de2017.df$q36 == "nein"] <- "INELIGIBLE"
de2017.df$vote2013[!is.na(de2017.df$q37) & de2017.df$q37 == "nein, habe nicht gewaehlt"] <- "ABSTAIN"
de2017.df$vote2013[is.na(de2017.df$vote2013)] <- "UNKNOWN"
table(de2017.df$vote2013)

df2017.svy <- svydesign(~1, data = de2017.df)



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



## ==== INITIAL TESTS ====

#Targets as named list, defined here
test1.svy <- quickRake(de2017.df, weightTargets = list(
    vote2013 = targets.vec$vote2013, ostwest = targets.vec$ostwest, q1 = targets.vec$gender))

#Targets as externally named list
targets.w8target <- list(
    vote2013 = as.w8target(targets.vec$vote2013, varname = "vote2013"),
    ostwest = as.w8target(targets.vec$ostwest, varname = "ostwest"),
    q1 = as.w8target(targets.vec$gender, varname = "q1")
)
test2.svy <- quickRake(de2017.df, weightTargets = targets.w8target)

#Targets as externally named list, using column name to define target
test3.svy <- quickRake(de2017.df, weightTargets = targets.w8target, weightTarget.id = "colname") #With names of list (generates warning as we are supplying two things)
test4.svy <- quickRake(de2017.df, weightTargets = list(targets.w8target$vote2013, targets.w8target$ostwest, targets.w8target$q1), weightTarget.id = "colname") #Without names of list (this is cleaner, as we aren't supplying two conflicting things)


## ==== THINGS TO TEST ====
#----Targets where column names clash with list names----
bad.w8target <- targets.w8target
names(bad.w8target$vote2013) <- c("pastvote", "Freq")
quickRake(de2017.df, weightTargets = bad.w8target) #With names of list (generates warning as we are supplying two things)

bad2.w8target <- list(past_vote = targets.w8target$vote2013, q1 = targets.w8target$q1, ostwest = targets.w8target$ostwest)
quickRake(de2017.df, weightTargets = bad2.w8target, weightTarget.id = "colname") #With names of list (generates warning as we are supplying two things)

#----single variable----
test <- quickRake(de2017.df, weightTargets = list(vote2013 = targets.w8target$vote2013))
test <- quickRake(de2017.df, weightTargets = list(vote2013 = targets.w8target$vote2013), weightTarget.id = "colname")
test <- quickRake(de2017.df, weightTargets = list(vote2013 = targets.vec$vote2013))
test <- quickRake(de2017.df, weightTargets = list(as.w8target(targets.vec$vote2013, varname = "vote2013")), weightTarget.id = "colname")
test <- quickRake(de2017.df, weightTargets = targets.vec$vote2013)
test <- quickRake(de2017.df, weightTargets = targets.vec$vote2013, weightTarget.id = "colname")
test <- quickRake(de2017.df, weightTargets = as.w8target(targets.vec$vote2013, varname = "vote2013"))
test <- quickRake(de2017.df, weightTargets = as.w8target(targets.vec$vote2013, varname = "vote2013"), weightTarget.id = "colname")



#----bad target and observed levels ----
#zero targets
vote2013_alt_target <- targets.vec$vote2013
vote2013_alt_target["INELIGIBLE"] <- 0
vote2013_alt_target["UNKNOWN"] <- 0 
vote2013_alt_target["ABSTAIN"] <- .285

targets_zero.w8target <- list(
    vote2013 = as.w8target(vote2013_alt_target , varname = "vote2013"),
    ostwest = as.w8target(targets.vec$ostwest, varname = "ostwest"),
    q1 = as.w8target(targets.vec$gender, varname = "q1")
)
test.svy <- quickRake(de2017.df, weightTargets = targets_zero.w8target)

#NA targets
vote2013_alt_target <- targets.vec$vote2013
vote2013_alt_target["INELIGIBLE"] <- NA
vote2013_alt_target["UNKNOWN"] <- NA
vote2013_alt_target["ABSTAIN"] <- .285
targets_NA.w8target <- list(
    vote2013 = as.w8target(vote2013_alt_target , varname = "vote2013"),
    ostwest = as.w8target(targets.vec$ostwest, varname = "ostwest"),
    q1 = as.w8target(targets.vec$gender, varname = "q1")
)
test.svy <- quickRake(de2017.df, weightTargets = list(ostwest = targets.vec$ostwest, q1 = targets.vec$gender,
                                                      vote2013 = vote2013_alt_target))


#zero observed
#NA observed

#----non-matching target and observed levels----
# REPEAT THIS with matchTargetsBy = name, order and refactor = TRUE, FALSE
#surplus levels (empty) in observed
#surplus levels (empty) in target
#surplus levels (non-empty) in observed
#surplus levels (non-empty) in target

#non-matching level names (equal number of levels)
#non-matching level names (unequal number of levels)
#two identical rows in target

#---- samplesize and rebasetolerance ----

    