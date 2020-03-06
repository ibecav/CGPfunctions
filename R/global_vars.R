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
    "nodeID",
    "adjustedp",
    "parent",
    "ruletext",
    "split.variable",
    "rawpvalue",
    "statistic",
    "parameter",
    "support",
    "logged",
    "sensible",
    "astext",
    "."
  ),
  package = "CGPfunctions",
  add = FALSE
)