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
#'   \item{q1}{gender}
#'   \item{q2a}{birth year}
#'   \item{q2b}{eligibility to vote in the (upcoming) 2017 German federal elections}
#'   \item{q11ba}{party the respondent plans to vote for in the upcoming (2017) election}
#'   \item{q36}{eligibility to vote in the (previous) 2013 German federal election}
#'   \item{q37}{whether the respondent actually voted in 2013}
#'   \item{vote2013}{respondent's reported vote in 2013 (specifically the 'second vote')}
#'   \item{ostwest}{whether the respondent lives in East or West Germany}
#'   \item{bula}{state the respondent lives in}
#'   ...
#' }
#' @source GLES data and documentation is available at \url{https://gles-en.eu/download-data/vor-und-nachwahlquerschnitt-2017/}.
#' Data is taken from the pre-election wave, file ZA6800, for a limited number of variables.
"de2017"
