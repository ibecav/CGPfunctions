#' @rdname newcancer
#' @title Tufte dataset on cancer survival rates
#' @aliases newcancer
#'
#' @description A dataset containing cancer survival rates for different types of cancer
#' over a 20 year period.
#'
#' @format A data frame with 96 rows and 3 variables:
#' \describe{
#'   \item{Year}{ordered factor for the 5, 10, 15 and 20 year survival rates}
#'   \item{Type}{factor containing the name of the cancer type}
#'   \item{Survival}{numeric for this data a whole number corresponding to the percent survival rate}
#'
#' }
#' @source \url{https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk}
#' @keywords datasets
"newcancer"

#' @rdname newgdp
#' @title Tufte dataset on Gross Domestic Product, 1970 and 1979
#' @aliases newgdp
#' @description Current receipts of fifteen national governments as a percentage of gross domestic product
#'
#' @format A data frame with 30 rows and 3 variables:
#' \describe{
#'   \item{Year}{character for 1970 and 1979}
#'   \item{Country}{factor country name}
#'   \item{GDP}{numeric a percentage of gross domestic product}
#'
#' }
#' @source Edward Tufte. \emph{Beautiful Evidence}. Graphics Press, 174-176.
#' @keywords datasets
"newgdp"

#' @rdname USvoteS
#' @title U.S. 2000 Election Data (short)
#' @aliases USvoteS
#' @description Data from a post-election survey following the year 2000 U.S. 
#'    presidential elections.  This is a subset from package `CHAID`.
#'
#' @format A data frame with 1000 observations on the following 6 variables.:
#' \describe{
#'   \item{vote3}{candidate voted for Gore or Bush}
#'   \item{gender}{gender, a factor with levels male and female}
#'   \item{ager}{age group, an ordered factor with levels 18-24 < 25-34 < 35-44 < 45-54 < 55-64 < 65+}
#'   \item{empstat}{status of employment, a factor with levels yes, no or retired}
#'   \item{educr}{status of education, an ordered factor with levels <HS < HS < >HS < College < Post Coll}
#'   \item{marstat}{status of living situation, a factor with levels married, widowed, divorced or never married}
#'
#' }
#' @source https://r-forge.r-project.org/R/?group_id=343
#' @keywords datasets
"USvoteS"

#' @rdname chaidUS
#' @title U.S. 2000 Election Data (short)
#' @aliases chaidUS
#' @description Data from a post-election survey following the year 2000 U.S. 
#'    presidential elections.  This is a subset from package `CHAID`.
#'
#' @format A partykit on the following 6 variables.:
#' \describe{
#'   \item{vote3}{candidate voted for Gore or Bush}
#'   \item{gender}{gender, a factor with levels male and female}
#'   \item{ager}{age group, an ordered factor with levels 18-24 < 25-34 < 35-44 < 45-54 < 55-64 < 65+}
#'   \item{empstat}{status of employment, a factor with levels yes, no or retired}
#'   \item{educr}{status of education, an ordered factor with levels <HS < HS < >HS < College < Post Coll}
#'   \item{marstat}{status of living situation, a factor with levels married, widowed, divorced or never married}
#'
#' }
#' @source https://r-forge.r-project.org/R/?group_id=343
#' @keywords datasets
"chaidUS"
