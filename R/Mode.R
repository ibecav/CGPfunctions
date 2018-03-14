#' Derive the modal value(s) for a set of data
#' 
#' This function takes a vector and returns one or mode values
#' that represent the mode point of the data
#'
#' @param x a vector 
#'
#' @return a vector containing one or more modal values for the input vector
#' @export
#'
#' @section Warning:
#' Be careful the function does some basic error checking but the return to
#' \code{Mode(NA)} is \code{NA} and a vector where the majority of entries
#' are \code{NA} is also NA
#' 
#' @examples
#' Mode(sample(1:100,1000,replace=TRUE))
#' Mode(mtcars$hp)
#' Mode(iris$Sepal.Length)
#' 
Mode <- function(x) {
  # error checking   
  if (missing(x)) {
    stop("Argument \"x\" is missing, with no default")
  }
  if (!is.vector(x)) {
    stop("I can only process a vector of data")
  }
  
  ux <- unique(x)
  #  ux[which.max(tabulate(match(x, ux)))]
  tab <- tabulate(match(x, ux)); ux[tab == max(tab)]
}

