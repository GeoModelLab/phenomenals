#' Sample Phenological Observations (BBCH) for Carmenere
#'
#' A small dataset of observed BBCH growth stages for the grape variety *Carmenere*
#' collected at the Colli Orientali site. Used in calibration of phenology models.
#'
#' @format A data frame with 6 rows and 6 columns:
#' \describe{
#'   \item{Variety}{Grape variety name (e.g., "Carmenere")}
#'   \item{Site}{Site name (e.g., "ColliOrientali")}
#'   \item{Latitude}{Latitude of the site (decimal degrees)}
#'   \item{Longitude}{Longitude of the site (decimal degrees)}
#'   \item{Date}{Observation date (class: Date)}
#'   \item{BBCH}{Observed BBCH stage (numeric)}
#' }
#'
#' @usage data(bbchSample)
#' @examples
#' data(bbchSample)
#' head(bbchSample)
"bbchSample"
