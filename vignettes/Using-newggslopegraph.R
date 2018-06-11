## ----setup---------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# Install from CRAN
# install.packages("CGPfunctions")

# Or the development version from GitHub
# install.packages("devtools")
# devtools::install_github("ibecav/CGPfunctions")
library(CGPfunctions)

## ----ggslope1, fig.height=10, fig.width=7--------------------------------
newggslopegraph(newcancer,Year,Survival,Type)

## ----ggslope2, fig.height=10, fig.width=7--------------------------------
newggslopegraph(dataframe = newcancer,
                Times = Year,
                Measurement = Survival,
                Grouping = Type,
                Title = "Estimates of Percent Survival Rates",
                SubTitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
                Caption = NULL
                )

## ----ggslope3, fig.height=5, fig.width=5---------------------------------
moredata <- structure(list(Date = structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L), 
                                            .Label = c("11-May-18", "18-May-18", "25-May-18"), 
                                            class = "factor"), 
                           Party = structure(c(5L, 3L, 2L, 1L, 4L, 5L, 3L, 2L, 1L, 4L, 5L, 3L, 2L, 1L, 4L), 
                                             .Label = c("Green", "Liberal", "NDP", "Others", "PC"), 
                                             class = "factor"), 
                           Pct = c(42.3, 28.4, 22.1, 5.4, 1.8, 41.9, 29.3, 22.3, 5, 1.4, 41.9, 26.8, 26.8, 5, 1.4)), 
                      class = "data.frame", 
                      row.names = c(NA, -15L))
#tail(moredata)
newggslopegraph(moredata,Date,Pct,Party, Title = "Notional data", SubTitle = NULL, Caption = NULL)

