## code to prepare `gles17` dataset goes here
library(foreign)
gles17 <- suppressWarnings(read.spss("data-raw/ZA6800_v5-0-0.sav", to.data.frame = TRUE))

# ---- Recode party/vote  variables ----
gles17$vote2013 <- factor(gles17$q38ba, levels = c(
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
gles17$vote2013[is.na(gles17$q36) | gles17$q36 == "nein"] <- "INELIGIBLE"
gles17$vote2013[!is.na(gles17$q37) & gles17$q37 == "nein, habe nicht gewaehlt"] <- "ABSTAIN"
gles17$vote2013[is.na(gles17$vote2013)] <- "UNKNOWN"

gles17$turnout2013 <- factor(gles17$q37, labels = c("voted", "did not vote"))
gles17$eligible2013 <- factor(gles17$q36, labels = c("eligible 2013", "ineligible 2013"))

gles17$votecurrent <- factor(gles17$q11ba, levels = c(
    "CDU/CSU", 
    "SPD", 
    "FDP",
    "GRUENE", 
    "DIE LINKE", 
    "AfD", 
    "andere Partei"
))

# ---- Recode education ----
gles17$educ <- addNA(factor(gles17$q136, labels = c(
    "Low",
    "Low",
    "Medium",
    "Medium",
    "High",
    "Medium",
    NA
)))

# ---- Recode other variables into English for readability ----
gles17$state <- gles17$bula
gles17$eastwest <- factor(gles17$ostwest, labels = c("East Germany", "West Germany"))
gles17$gender <- factor(gles17$q1, labels = c("Male", "Female"))
gles17$birthyear <- gles17$q2a
gles17$votingage <- factor(gles17$q2d, labels = c("ineligible", "eligible"))
gles17$dweight <- gles17$w_ipfges
gles17$hhsize <- as.numeric(gles17$q132)
gles17$agecat <- factor(
    cut(2017 - gles17$q2a, breaks = c(0, 30, 40, 50, 60, 70, 100)),
    labels = c("<=29", "30-39", "40-49", "50-59", "60-69", ">=70")
)
gles17$gender_educ <- interaction(gles17$gender, gles17$educ, sep = ":")
    
# ---- Note on key variables ----
#bula - state
#ostwest - state location
#q1 - gender
#q2a - birth year
#q2d - eligible to vote
#q11ba - vote intent
#q36 - eligible in 2013
#37 - voted in 2013
#q38ba - 2013 vote
#intnum - interviewer code
#vpoint - sampling unit
#dweight - design weight
#hhsize - number of people in household
#educ - simple redoce of education
#gender_educ - interaction of gender and education
gles17 <- gles17[, c(
    "gender", 
    "educ",
    "gender_educ",
    "birthyear",  
    "votingage", 
    "state", 
    "eastwest", 
    "vote2013", 
    "turnout2013", 
    "eligible2013", 
    "votecurrent",
    "intnum",
    "vpoint",
    "hhsize",
    "dweight"
    )]

usethis::use_data(gles17, overwrite = TRUE)
