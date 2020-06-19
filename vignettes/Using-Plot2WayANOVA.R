## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = FALSE-------------------------------------------------------------
#  # Install from CRAN
#  install.packages("CGPfunctions")
#  
#  # Or the development version from GitHub
#  # install.packages("devtools")
#  devtools::install_github("ibecav/CGPfunctions")

## ----LoadLibrary, warning = FALSE---------------------------------------------
library(CGPfunctions)

## ----Plot2WayANOVA, echo=TRUE, message=TRUE, warning=FALSE, fig.width=7, fig.height=4----
Plot2WayANOVA(formula = mpg ~ am * cyl, dataframe = mtcars)

## ----Plot2WayANOVA2, echo=TRUE, fig.height=4, fig.width=7, message=FALSE, warning=FALSE----
Plot2WayANOVA(formula = mpg ~ cyl * am, 
              dataframe = mtcars,
              confidence = .99,
              title = "MPG by cylinders and type transmission",
              xlab = "Cylinders",
              ylab = "Miles per gallon",
              mean.label = TRUE,
              mean.shape = 22,
              posthoc.method = "lsd",
              errorbar.display = "SEM"
              )

## ----Plot2WayANOVA3, echo=TRUE, fig.height=4, fig.width=7, message=FALSE, warning=FALSE----
# Create a new dataset
library(dplyr)
library(ggplot2)
library(stringi)
newmpg <- mpg %>% 
          filter(cyl != 5) %>% 
          mutate(am = stringi::stri_extract(trans, regex = "auto|manual"))
Plot2WayANOVA(formula = hwy ~ am * cyl,
              dataframe = newmpg,
              ylab = "Highway mileage",
              xlab = "Transmission type",
              plottype = "line",
              offset.style = "wide",
              overlay.type = "box",
              mean.label = TRUE, 
              mean.shape = 20, 
              mean.size = 3, 
              mean.label.size = 3,
              show.dots = TRUE,
              errorbar.display = "SD",
              ggtheme = ggplot2::theme_minimal(),
              ggplot.component = theme(axis.text.x = element_text(size=14, color="darkblue"))
              )

