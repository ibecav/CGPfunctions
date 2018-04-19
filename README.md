
<!-- README.md is generated from this README.Rmd. Please edit the .rmd file not the .md file -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/CGPfunctions)](https://CRAN.R-project.org/package=CGPfunctions)

## Overview

A package that includes functions that I find useful for teaching
statistics as well as actually practicing the art. They typically are
not “new” methods but rather wrappers around either base R or other
packages and concepts I’m trying to master. Currently contains:

  - `Plot2WayANOVA` which as the name implies conducts a 2 way ANOVA and
    plots the results using `ggplot2`
  - `PlotXTabs` which as the name implies plots cross tabulated
    variables using `ggplot2`
  - `neweta` which is a helper function that appends the results of a
    Type II eta squared calculation onto a classic ANOVA table
  - `Mode` which finds the modal value in a vector of data
  - `SeeDist` which wraps around ggplot2 to provide visualizations of
    univariate data.
  - `OurConf` is a simulation function that helps you learn about
    confidence intervals

## Installation

``` r
# Install from CRAN
install.packages("CGPfunctions")

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("ibecav/CGPfunctions")
```

## Usage

`library(CGPfunctions)` will load the package which contains 4
functions:

`SeeDist` will give you some plots of the distribution of a variable
using `ggplot2`

`Mode` is a helper function that simply returns one or more modal values

`neweta` is a helper function which returns a tibble containing AOV
output similar to summary(aov(MyAOV)) but with eta squared computed and
appended as an additional column

The `Plot2WayANOVA` function conducts a classic analysis using existing
R functions and packages in a sane and defensible manner not necessarily
in the one and only manner.

`OurConf` is a simulation function that helps you learn about confidence
intervals

## Credits

Many thanks to Dani Navarro and the book \> ([Learning Statistics with
R](http://www.compcogscisydney.com/learning-statistics-with-r.html))
whose etaSquared function was the genesis of `neweta`.

“He who gives up safety for speed deserves neither.”
([via](https://twitter.com/hadleywickham/status/504368538874703872))

#### A shoutout to some other packages I find essential.

  - [stringr](https://github.com/tidyverse/stringr), for strings.
  - [lubridate](https://github.com/hadley/lubridate), for date/times.
  - [forcats](https://github.com/hadley/forcats), for factors.
  - [haven](https://github.com/hadley/haven), for SPSS, SAS and Stata
    files.
  - [readxl](https://github.com/hadley/readxl), for `.xls` and `.xlsx`
    files.
  - [modelr](https://github.com/hadley/modelr), for modelling within a
    pipeline
  - [broom](https://github.com/dgrtwo/broom), for turning models into
    tidy data
  - [ggplot2](http://ggplot2.tidyverse.org), for data visualisation.
  - [dplyr](http://dplyr.tidyverse.org), for data manipulation.
  - [tidyr](http://tidyr.tidyverse.org), for data tidying.
  - [readr](http://readr.tidyverse.org), for data import.
  - [purrr](http://purrr.tidyverse.org), for functional programming.
  - [tibble](http://tibble.tidyverse.org), for tibbles, a modern
    re-imagining of data frames.

## Leaving Feedback

If you like **CGPfunctions**, please consider leaving [feedback
here](https://github.com/ibecav/CGPfunctions/issues).

## Contributing

Contributions in the form of feedback, comments, code, and bug reports
are most welcome. How to contribute:

  - Issues, bug reports, and wish lists: [File a GitHub
    issue](https://github.com/ibecav/CGPfunctions/issues).
  - Contact the maintainer ibecav at gmail.com by email.
