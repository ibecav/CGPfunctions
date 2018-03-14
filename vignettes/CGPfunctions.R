## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(CGPfunctions::neweta(aov(mpg~am*cyl, mtcars)))

## ----echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE---------
xxx<-CGPfunctions::Plot2WayANOVA(mpg~am*cyl, mtcars)

