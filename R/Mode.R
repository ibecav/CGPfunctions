######
#' Derive the modal value(s) for a set of data
#' 
#' This function takes a set of numeric data and returns one or mode values
#' that represent the mode point of the data
#'
#' @param x a vector 
#'
#' @return a list
#' @export
#'
#' @examples
#' Mode(mtcars$hp)
#' Mode(iris$Sepal.Length)
#' 
Mode <- function(x) {
  ux <- unique(x)
  #  ux[which.max(tabulate(match(x, ux)))]
  tab <- tabulate(match(x, ux)); ux[tab == max(tab)]
}

