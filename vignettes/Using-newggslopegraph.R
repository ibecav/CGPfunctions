## ----setup--------------------------------------------------------------------
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
library(tidyr)
library(dplyr)

## ----ggslope1, fig.height=10, fig.width=7-------------------------------------
newggslopegraph(newcancer,Year,Survival,Type)

## ----ggslope2, fig.height=10, fig.width=7-------------------------------------
newggslopegraph(dataframe = newcancer,
                Times = Year,
                Measurement = Survival,
                Grouping = Type,
                Title = "Estimates of Percent Survival Rates",
                SubTitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
                Caption = NULL
                )

## ----ggslope3, fig.height=5, fig.width=5--------------------------------------
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

## ----ggslope4, fig.height=5, fig.width=5--------------------------------------
newggslopegraph(moredata, Date, Pct, Party, 
                Title = "Notional data", 
                SubTitle = "none", 
                Caption = "imaginary",
                LineColor = "gray", 
                LineThickness = .5,
                YTextSize = 4
                )

## ----ggslope5, fig.height=5, fig.width=5--------------------------------------
newggslopegraph(moredata, Date, Pct, Party, 
                Title = "Notional data", 
                SubTitle = "none", 
                Caption = "imaginary",
                LineColor = c("Green" = "gray", "Liberal" = "green", "NDP" = "red", "Others" = "gray", "PC" = "gray"), 
                LineThickness = .5,
                YTextSize = 4
                )

## ----ggslope6, fig.height=12, fig.width=6-------------------------------------
newggslopegraph(newgdp, 
                Year, 
                GDP, 
                Country, 
                Title = "Gross GDP", 
                SubTitle = NULL, 
                Caption = NULL,
                LineThickness = .5,
                YTextSize = 4,
                LineColor = c(rep("gray",3), "red", rep("gray",3), "red", rep("gray",10))
                )

## ----ggslope7, fig.height=7, fig.width=6--------------------------------------
newgdp$rGDP <- signif(newgdp$GDP, 2)
newggslopegraph(newgdp, 
                Year, 
                rGDP, 
                Country, 
                Title = "Gross GDP", 
                SubTitle = NULL, 
                Caption = NULL,
                LineThickness = .5,
                YTextSize = 4,
                LineColor = c(rep("gray",6), rep("red",2), "red", rep("gray",10))
                )

custom_colors <- tidyr::pivot_wider(newgdp, 
                   id_cols = Country, 
                   names_from = Year, 
                   values_from = GDP) %>% 
  mutate(difference = Year1979 - Year1970) %>%
  mutate(trend = case_when(
    difference >= 2 ~ "green",
    difference <= -1 ~ "red",
    TRUE ~ "gray"
    )
  ) %>%
  select(Country, trend) %>%
  tibble::deframe()

custom_colors

newggslopegraph(newgdp, 
                Year, 
                rGDP, 
                Country, 
                Title = "Gross GDP", 
                SubTitle = NULL, 
                Caption = NULL,
                LineThickness = .5,
                YTextSize = 4,
                LineColor = custom_colors
)


