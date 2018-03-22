## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "P2A-",
  fig.dim = c(6, 4)
)

## ----eval = FALSE--------------------------------------------------------
#  # Install from CRAN
#  install.packages("CGPfunctions")
#  
#  # Or the development version from GitHub
#  # install.packages("devtools")
#  devtools::install_github("ibecav/CGPfunctions")

## ----LoadLibrary---------------------------------------------------------
library(CGPfunctions)

## ----Plot2WayANOVA, echo=TRUE, message=TRUE, warning=FALSE, paged.print=TRUE----
Plot2WayANOVA(mpg~am*cyl, mtcars)

