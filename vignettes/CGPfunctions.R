## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE---------
xxx<-CGPfunctions::Plot2WayANOVA(mpg~am*cyl, mtcars)

