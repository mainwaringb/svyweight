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

# ---- Recode other variavles into English for readability ----
gles17$state <- gles17$bula
gles17$eastwest <- factor(gles17$ostwest, labels = c("East Germany", "West Germany"))
gles17$gender <- factor(gles17$q1, labels = c("Male", "Female"))
gles17$birthyear <- gles17$q2a
gles17$votingage <- factor(gles17$q2d, labels = c("ineligible", "eligible"))
    
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
gles17 <- gles17[, c("gender", "birthyear",  "votingage", "state", "eastwest", "vote2013", "turnout2013", "eligible2013", "votecurrent")]

usethis::use_data(gles17, overwrite = TRUE)
