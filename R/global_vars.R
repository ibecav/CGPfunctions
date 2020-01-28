# defining global variables and functions to appease R CMD Check

utils::globalVariables(
  names = c(
    ".",
    "perc",
    "N",
    "n",
    "TheMean",
    "TheSEM",
    "CIMuliplier",
    "LowerBound",
    "UpperBound",
    "p.value",
    "term",
    "."
  ),
  package = "CGPfunctions",
  add = FALSE
)