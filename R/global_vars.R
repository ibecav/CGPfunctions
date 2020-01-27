# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    "perc",
    "N"
  ),
  package = "CGPfunctions",
  add = FALSE
)