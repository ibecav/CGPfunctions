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
