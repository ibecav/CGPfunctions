## ----setup, echo = FALSE, warning=FALSE, message=FALSE------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(CGPfunctions)

## ----LoadLibrary--------------------------------------------------------------
library(productplots)
str(happy)

## ----vignette1, fig.width=6.0, fig.height=2.5---------------------------------
# who's happier by gender
PlotXTabs(happy,happy,sex)
# same thing using column numbers and a stacked bar
PlotXTabs(happy,2,5,"stack")
# happiness by a variety of possible factors as a percent
PlotXTabs(happy, 2, c(5:9), plottype = "percent")
# turn the numbers around and change them up basically just showing all
# the permutations
PlotXTabs(happy, c(2,5), 9, plottype = "side")
PlotXTabs(happy, c(2,5), c(6:9), plottype = "percent")
PlotXTabs(happy, happy, c(6,7,9), plottype = "percent")
PlotXTabs(happy, c(6,7,9), happy, plottype = "percent")

