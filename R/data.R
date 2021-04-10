#' Partial Data from the 2017 German Election Survey
#' 
#' Partial data from the pre-election 2017 wave of the German Longitudinal Election Study (GLES).
#' Includes variables for vote in the 2013 German federal election to the Bundestag
#' (lower house of parliament) - specifically the 'second vote'. Also includes other demographics that
#' might be used for weighting, such as gender, birth year, and state. 
#' Each row in the dataset is a unique respondent who completed the survey.
#'
#' @format A data frame with 2179 rows and 11 columns:
#' \describe{
#'   \item{gender}{gender}
#'   \item{educ}{educational attainment, based on kind of secondary school from which respondent graduated}
#'   \item{gender_educ}{interaction of gender and education attainment}
#'   \item{birthyear}{four-digit birth year}
#'   \item{votingage}{eligibility to vote in the (upcoming) 2017 German federal elections}
#'   \item{agecat}{approximate age category in 2017, estimated from birth year}
#'   \item{state}{state the respondent lives in}
#'   \item{eastwest}{whether the respondent lives in East or West Germany}
#'   \item{vote2013}{respondent's reported vote in 2013 (specifically the 'second vote')}
#'   \item{turnout2013}{whether the respondent actually voted in 2013}
#'   \item{votecurrent}{party the respondent plans to vote for in the upcoming (2017) election}
#'   \item{intnum}{unique code for the interviewer who conducted an interview}
#'   \item{vpoint}{unique code anonymously identifying census block where an interview was conducted}
#'   \item{hhsize}{number of people in the household}
#'   \item{dweight}{nationally-representative design weight supplied by the GLES study authors}
#'   ...
#' }
#' @source GLES data and documentation is available at \url{https://gles-en.eu/download-data/vor-und-nachwahlquerschnitt-2017/}.
#' Data is taken from the pre-election wave, file ZA6800, for a limited number of variables. Note that most documentation is available in English,
#' but some may be in German only.
"gles17"

