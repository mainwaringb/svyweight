## ==== Set up example data (2017 German Election Study) ====
# Currently this is copy-and-pasted from test_rake.R

# ---- Define datasets and variables ----
# Flip order of levels for one variable
gles17_flipped_level.df <- gles17
gles17_flipped_level.df$eastwest <- factor(gles17$eastwest, levels = c("West Germany", "East Germany"))

# Subset to drop all cases of one level
no_unknowns_10cat.df <- gles17[gles17$vote2013 != "UNKNOWN",] # keep level in definition of factor
no_unknowns_9cat.df  <- no_unknowns_10cat.df
no_unknowns_9cat.df$vote2013 <- factor(no_unknowns_9cat.df$vote2013)

# Create versions of dataset with implicit and explicit NAs
implicit_na.df <- gles17
implicit_na.df$eastwest[1:50] <- NA
explicit_na.df <- implicit_na.df
explicit_na.df$eastwest <- addNA(implicit_na.df$eastwest)

# Create svydesign object
gles17.svy <- suppressWarnings(survey::svydesign(~1, data = gles17))

# Bad dataframe where one level consists only of cases with zero design weights
zero_dweights <- weights(gles17.svy)
zero_dweights[gles17.svy$variables$vote2013 == "INELIGIBLE"] <- 0
zero_dweights[1:50] <- 0
gles17_zero_dweight.svy <- suppressWarnings(survey::svydesign(~1, weights = zero_dweights, data = gles17))

# Bad dataframe where one levels consists only of cases that will be dropped due to a 0% target on another variable
gles17_bad_level.df <- gles17
gles17_bad_level.df$eastwest <- "West Germany"
gles17_bad_level.df$eastwest[gles17$vote2013 == "UNKNOWN"] <- "East Germany"
gles17_bad_level.df$eastwest <- factor(gles17_bad_level.df$eastwest, levels = levels(gles17$eastwest))
gles17_bad_level.svy <- suppressWarnings(survey::svydesign(~1, data = gles17_bad_level.df))


## ==== Set up example target numbers ====

# ---- Define main targets ----
#Define targets (note that these targets may not be accurate - they are examples only)
targets.vec <- list(
    vote2013 = c(
        "CDU/CSU" = .297,
        "SPD" = .184,
        "FDP" = .034,
        "GRUENE" = .060,
        "DIE LINKE" = .061,
        "AfD" = .034,
        "andere Partei" = .045,
        "ABSTAIN" = .185,
        "INELIGIBLE" = .050,
        "UNKNOWN" = .050),
    eastwest = c(
        "East Germany" = .195,
        "West Germany" = .805),
    gender = c(
        "Male" = .495,
        "Female" = .505)
)

# ---- define alternate targets  ----
# Targets with flipped level order
targets.vec$eastwest_reorder <- c(targets.vec$eastwest[2], targets.vec$eastwest[1])

# Targets with valid zeroes
targets.vec$vote2013_zero <- targets.vec$vote2013
targets.vec$vote2013_zero["INELIGIBLE"] <-.040
targets.vec$vote2013_zero["UNKNOWN"] <- 0
targets.vec$vote2013_zero["ABSTAIN"] <- .245

# Targets with NA *values*
targets.vec$vote2013_na <- targets.vec$vote2013
targets.vec$vote2013_na["INELIGIBLE"] <- NA
targets.vec$vote2013_na["UNKNOWN"] <- NA
targets.vec$vote2013_na["ABSTAIN"] <- .285

# Targets entirely omitting one level
targets.vec$vote2013_known <- targets.vec$vote2013[names(targets.vec$vote2013) != "UNKNOWN"]
targets.vec$vote2013_known["ABSTAIN"] <- .245
targets.vec$vote2013_known["INELIGIBLE"] <- .040

# Targets with level names altered (using English-language names for parties)
targets.vec$vote2013_en <- c(
    "CDU/CSU" = .297,
    "SPD" = .184,
    "FDP" = .034,
    "GREEN" = .060,
    "LEFT" = .061,
    "AfD" = .034,
    "OTHER" = .045,
    "ABSTAIN" = .185,
    "INELIGIBLE" = .050,
    "UNKNOWN" = .050)
targets.vec$vote2013_en_known <- targets.vec$vote2013_en[names(targets.vec$vote2013_en) != "UNKNOWN"]
targets.vec$vote2013_en_known["ABSTAIN"] <- .245
targets.vec$vote2013_en_known["INELIGIBLE"] <- .040

# intentionally creating a bad target, with an unused level with no cases
targets.vec$vote2013_zero_bad <- targets.vec$vote2013_zero
targets.vec$vote2013_zero_bad["ASDF"] <- 0

# Creating a target with a new level named NA
# Note that this is hashed out, bc it won't convert to as.w8margin
targets.vec$eastwest_implicit_na <- targets.vec$eastwest
targets.vec$eastwest_implicit_na <- c(targets.vec$eastwest, .01)
targets.vec$eastwest_implicit_na["West Germany"] <- .795
names(targets.vec$eastwest_implicit_na)[3] <- NA

# ---- Define matrix and data frame formats ----
# Matrix target
targets.mat <- list()
targets.mat$gender_educ_all <- matrix(
    c(.15, .17, .17, .01, .19, .16, .14, .01),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("Male", "Female"), c("Low", "Medium", "High", NA)))
targets.mat$gender_educ_valid <- targets.mat$gender_educ[, c(1:3)]

# Main data frame targets
targets.df <- list()
targets.df$vote2013 <- data.frame(
    vote2013 = names(targets.vec$vote2013),
    Freq = targets.vec$vote2013
)

# Problematic data frame targets
targets.df$vote2013_name_plus_col <- targets.df$vote2013
rownames(targets.df$vote2013_name_plus_col) <- names(targets.vec$vote2013_en)
targets.df$vote2013_name_only <- data.frame(targets.df$vote2013)

targets.df$vote2013_extra_col <- cbind(targets.df$vote2013, Freq_known = c(targets.vec$vote2013_known, 0))
targets.df$vote2013_col_names_flipped <- targets.df$vote2013[,c(2,1)]

targets.df$vote2013_wrong_name_freq <- targets.df$vote2013
colnames(targets.df$vote2013_wrong_name_freq)[2] <- "Count"
targets.df$vote2013_wrong_name_cats <- targets.df$vote2013
colnames(targets.df$vote2013_wrong_name_freq)[1] <- "pastvote"

targets.df$vote2013_na <- data.frame(
    vote2013 = names(targets.vec$vote2013_na),
    Freq = targets.vec$vote2013_na
)

## ==== CREATE BENCHMARK OBJECTS ====

# Don' rerun this code - we want to have static w8margin objects and weights to test against
# If we do need to rerun, need to change "overwrite = TRUE" in use_data (and )
# 
# all.w8margin <- list()
# all.w8margin$vote2013 <- as.w8margin(targets.vec$vote2013, varname = "vote2013")
# all.w8margin$eastwest <- as.w8margin(targets.vec$eastwest, varname = "eastwest")
# all.w8margin$gender <- as.w8margin(targets.vec$gender, varname = "gender")
# all.w8margin$vote2013_zero <- as.w8margin(targets.vec$vote2013_zero, varname = "vote2013")
# all.w8margin$vote2013_known <- as.w8margin(targets.vec$vote2013_known, varname = "vote2013")
# all.w8margin$vote2013_en <- as.w8margin(targets.vec$vote2013_en, varname = "vote2013")
# all.w8margin$vote2013_en_known <- as.w8margin(targets.vec$vote2013_en_known, varname = "vote2013")
# all.w8margin$eastwest_reorder <- as.w8margin(targets.vec$eastwest_reorder, varname = "eastwest")
# all.w8margin$vote2013_zero_bad <- as.w8margin(targets.vec$vote2013_zero_bad , varname = "vote2013")
# all.w8margin$vote2013_na <- as.w8margin(targets.vec$vote2013_na, varname = "vote2013", na.allow = TRUE)
# 
# benchmark_out <- rakew8(gles17,
#                         vote2013 ~ all.w8margin$vote2013,
#                         eastwest ~ all.w8margin$eastwest,
#                         gender ~ all.w8margin$gender)
# benchmark_onevar_out <- rakew8(gles17,
#                   vote2013 ~ all.w8margin$vote2013)
# saveRDS(all.w8margin, file = "tests/testthat/all_w8margin.rds", compress = FALSE)
# saveRDS(benchmark_out, file = "tests/testthat/benchmark_out.rds", compress = FALSE)
# saveRDS(benchmark_onevar_out, file = "tests/testthat/benchmark_onevar_out.rds", compress = FALSE)


# usethis::use_data(all.w8margin, benchmark_out, benchmark_onevar_out, internal = TRUE, overwrite = FALSE)
all.w8margin <- readRDS("all_w8margin.rds")
benchmark_out <- readRDS("benchmark_out.rds")
benchmark_onevar_out <- readRDS("benchmark_onevar_out.rds")


# ==== CREATE LISTS OF W8MARGIN OBJECTS ====

# ---- main w8margin object ----
targets_main.w8margin <- list(
    vote2013 = all.w8margin$vote2013,
    eastwest = all.w8margin$eastwest,
    gender = all.w8margin$gender
)

# ---- Modified but useful w8margin objects ----
# Target for a quota of zero on some variable categories
targets_zero.w8margin <- list(
    vote2013 = all.w8margin$vote2013_zero,
    eastwest = all.w8margin$eastwest,
    gender = all.w8margin$gender
)

# Target omitting some variable categories
targets_known.w8margin <- targets_main.w8margin
targets_known.w8margin$vote2013 <- all.w8margin$vote2013_known

# Targets changing level names (using English-language translations of party names)
targets_en.w8margin <- list(
    vote2013 = all.w8margin$vote2013_en,
    eastwest = all.w8margin$eastwest,
    gender = all.w8margin$gender
)
targets_en_known.w8margin <- list(
    vote2013 = all.w8margin$vote2013_en_known,
    eastwest = all.w8margin$eastwest,
    gender = all.w8margin$gender
)

# Targets changing order of levels
targets_reorder.w8margin <- list( #match.levels.by = name
    vote2013 = all.w8margin$vote2013,
    eastwest = all.w8margin$eastwest_reorder,
    gender = all.w8margin$gender
)

# Adding NAs
targets_na.w8margin <- list(
    vote2013 = all.w8margin$vote2013_na,
    eastwest = all.w8margin$eastwest,
    gender = all.w8margin$gender
)

# ---- define intentionally problematic targets ----
bad_colnames.w8margin <- targets_main.w8margin
names(bad_colnames.w8margin$vote2013) <- c("pastvote", "Freq")

bad_zero_level.w8margin <- list(
    vote2013 = all.w8margin$vote2013_zero_bad,
    eastwest = all.w8margin$eastwest,
    gender = all.w8margin$gender
)

bad_listnames.w8margin <- list(
    past_vote = all.w8margin$vote2013,
    eastwest = all.w8margin$eastwest,
    gender = all.w8margin$gender
)


# ---- define targets with NA *level* ----
implicit_zero_target.w8margin <- targets_main.w8margin
implicit_zero_target.w8margin$eastwest <- rbind(targets_main.w8margin$eastwest, c(NA, .01))
implicit_zero_target.w8margin$eastwest$Freq <- c(.185, .785, .030)

explicit_zero_target.w8margin <- implicit_zero_target.w8margin
explicit_zero_target.w8margin$eastwest$eastwest <- addNA(explicit_zero_target.w8margin$eastwest$eastwest)
