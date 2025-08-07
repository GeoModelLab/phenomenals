#' Sample Yield Data for Carmenere (Colli Orientali)
#'
#' This dataset contains sample grape yield observations for the variety
#' *Carmenere* collected at the Colli Orientali site. It is used to test
#' the trait modeling capabilities of the `phenomenals` package.
#'
#' @format A data frame with 6 rows and 7 columns:
#' \describe{
#'   \item{Site}{Site name (e.g., "ColliOrientali")}
#'   \item{latitude}{Site latitude (decimal degrees)}
#'   \item{longitude}{Site longitude (decimal degrees)}
#'   \item{Variety}{Grape variety (e.g., "Carmenere")}
#'   \item{Year}{Year of observation}
#'   \item{Variable}{Trait name (here: "yield")}
#'   \item{Value}{Observed value of the trait (numeric)}
#'   \item{Unit}{Unit of measurement (e.g., "kg plant-1")}
#' }
#'
#' @usage data(targetSample)
#' @examples
#' data(targetSample)
#' head(targetSample)
"targetSample"
