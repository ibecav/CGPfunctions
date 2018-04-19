## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
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

## ----Plot2WayANOVA, echo=TRUE, message=TRUE, warning=FALSE, fig.width=6.0, fig.height=3.5----
Plot2WayANOVA(mpg~am*cyl, mtcars)

