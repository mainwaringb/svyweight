## code to prepare `de2017` dataset goes here
library(foreign)
de2017 <- suppressWarnings(read.spss("data-raw/ZA6800_v5-0-0.sav", to.data.frame = TRUE))

#Recode variables for weighting
de2017$vote2013 <- factor(de2017$q38ba, levels = c(
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
de2017$vote2013[is.na(de2017$q36) | de2017$q36 == "nein"] <- "INELIGIBLE"
de2017$vote2013[!is.na(de2017$q37) & de2017$q37 == "nein, habe nicht gewaehlt"] <- "ABSTAIN"
de2017$vote2013[is.na(de2017$vote2013)] <- "UNKNOWN"
table(de2017$vote2013)

# ---- Note on key variables ----
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
de2017 <- de2017[,c("q1", "q2a", "q2b", "q2d", "q11ba", "q36", "q37", "q38ba","ostwest", "bula", "vote2013")]

usethis::use_data(de2017, overwrite = TRUE)
