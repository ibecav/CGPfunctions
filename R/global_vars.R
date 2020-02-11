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
    "outcome",
    "splitrule",
    "df",
    "chisq",
    "method",
    "NodeN",
    "NodeID",
    "adjustedp",
    "parent",
    "ruletext",
    "split.value",
    "rawpvalue",
    "."
  ),
  package = "CGPfunctions",
  add = FALSE
)