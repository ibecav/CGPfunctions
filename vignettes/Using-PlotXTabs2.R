## ----setup, echo = FALSE, warning=FALSE, message=FALSE------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(CGPfunctions)

## ----simple, fig.width=7.0, fig.height=3.5------------------------------------
# simplest possible call with the defaults
PlotXTabs2(
  data = mtcars,
  y = am,
  x =  cyl
)  

## ----simple2, fig.width=7.0, fig.height=3.5-----------------------------------
# more complex call
PlotXTabs2(
  data = datasets::mtcars,
  y = am,
  x = cyl,
  bf.details = TRUE,
  xlab = "Number of cylinders",
  ylab = NULL,
  data.label = "both",
  label.fill.alpha = .3,
  labels.legend = c("0 = Manual", "1 = Automatic"),
  legend.title = "Transmission Type",
  legend.position = "left",
  title = "The perenial mtcars example",
  palette = "Pastel1"
)

## ----LoadLibrary--------------------------------------------------------------
library(dplyr)
library(purrr)
library(productplots)
library(tidyselect) 
str(happy)

## ----vignette1, fig.width=7.0, fig.height=3.5---------------------------------
# who's happier by gender
PlotXTabs2(happy,happy,sex)

## ----vignette2, fig.width=7.0, fig.height=3.5---------------------------------
myvariables <- happy %>% 
                select_if(is.factor)  %>% 
                select(-happy) %>% 
                names
mytitles <- stringr::str_c("Happiness by ", 
                           stringr::str_to_title(myvariables), 
                           " status")

myvariables
mytitles

purrr::map2(.x = myvariables,
            .y = mytitles,
            .f =  ~ PlotXTabs2(x = all_of(.x),
                               title = .y,
                               data = happy,
                               y = happy,
                               legend.title = "Rating",
                               xlab = stringr::str_to_title(.x),
                               ylab = NULL,
                               perc.k = 1,
                               palette = "Set2"
                               )
            )

