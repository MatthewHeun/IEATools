#' International Energy Agency data
#'
#' A dataset containing all IEA data after it has been "fixed"
#' to the best of our ability.  
#' See \code{IEAData::AllIEAData_prior_to_fix} for the same data before it has been "fixed."
#'
#' @format A data frame with 719,473 rows and 9 variables:
#' \describe{
#'   \item{Country}{Two-letter ISO country codes or "World" for the entire world.}
#'   \item{Ledger.side}{Supply or Consumption}
#'   \item{Flow.aggregation.point}{tells where each row should be aggregated}
#'   \item{Flow}{the Industry or Sector involved in this flow}
#'   \item{Product}{the energy product involved in this flow}
#'   \item{Year}{the year for this flow}
#'   \item{E.ktoe}{the magnitude of this energy flow}
#' }
#' @source \url{http://http://www.iea.org}
"AllIEAData"


#' Raw International Energy Agency data
#'
#' A dataset containing all IEA data before it has been "fixed"
#' to the best of our ability.  
#' See \code{IEAData::AllIEAData} for the same data after it has been "fixed."
#'
#' @format A data frame with 700,111 rows and 9 variables:
#' \describe{
#'   \item{Country}{Two-letter ISO country codes or "World" for the entire world.}
#'   \item{Ledger.side}{Supply or Consumption}
#'   \item{Flow.aggregation.point}{tells where each row should be aggregated}
#'   \item{Flow}{the Industry or Sector involved in this flow}
#'   \item{Product}{the energy product involved in this flow}
#'   \item{Year}{the year for this flow}
#'   \item{E.ktoe}{the magnitude of this energy flow}
#' }
#' @source \url{http://http://www.iea.org}
"AllIEAData_prior_to_fix"
