
<!-- README.md is generated from this README.Rmd. Please edit the .rmd file not the .md file -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/CGPfunctions)](https://CRAN.R-project.org/package=CGPfunctions)
[![Build
Status](https://travis-ci.org/ibecav/CGPfunctions.svg?branch=master)](https://travis-ci.org/ibecav/CGPfunctions)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/CGPfunctions)](http://cranlogs.r-pkg.org/badges/grand-total/CGPfunctions)

### Overview

A package that includes functions that I find useful for teaching
statistics as well as actually practicing the art. They typically are
not “new” methods but rather wrappers around either base R or other
packages and concepts I’m trying to master. Currently contains:

  - `Plot2WayANOVA` which as the name implies conducts a 2 way ANOVA and
    plots the results using `ggplot2`
  - `PlotXTabs` which as the name implies plots cross tabulated
    variables using `ggplot2`
  - `PlotXTabs2` which wraps around ggplot2 to provide Bivariate bar
    charts for categorical and ordinal data.
  - `newggslopegraph` which creates a Tufte“esque” slopegraph using
    `ggplot2`
  - `chaid_table` provides tabular summary of `CHAID` `partykit` object
  - `cross2_var_vectors` a helper function to build lists of “crossed”
    variables
  - `Mode` which finds the modal value(s) in a vector of data
  - `SeeDist` which wraps around ggplot2 to provide visualizations of
    univariate data.
  - `OurConf` is a simulation function that helps you learn about
    confidence intervals

### Installation

``` r
# Install from CRAN
install.packages("CGPfunctions")

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("ibecav/CGPfunctions")
```

### Usage

[Online documentation and vignettes are located
here](https://ibecav.github.io/CGPfunctions).

#### Example [Plot2WayANOVA](https://ibecav.github.io/CGPfunctions/articles/Using-Plot2WayANOVA.html) plot

<div class="figure">

<img src="https://ibecav.github.io/CGPfunctions/articles/Using-Plot2WayANOVA_files/figure-html/Plot2WayANOVA3-1.png" alt="Example Plot2WayANOVA" width="100%" />

<p class="caption">

Example Plot2WayANOVA

</p>

</div>

#### Example [PlotXTabs2](https://ibecav.github.io/CGPfunctions/articles/Using-PlotXTabs2.html) plot

<div class="figure">

<img src="https://ibecav.github.io/CGPfunctions/articles/Using-PlotXTabs2_files/figure-html/vignette2-2.png" alt="Example PlotXTabs2 plot" width="100%" />

<p class="caption">

Example PlotXTabs2 plot

</p>

</div>

#### Example [newggslopegraph](https://ibecav.github.io/CGPfunctions/articles/Using-newggslopegraph.html) plot

<div class="figure">

<img src="https://ibecav.github.io/CGPfunctions/articles/Using-newggslopegraph_files/figure-html/ggslope7-1.png" alt="Example newggslopegraph plot" width="80%" />

<p class="caption">

Example newggslopegraph plot

</p>

</div>

### Credits

Many thanks to Danielle Navarro and the book *[Learning Statistics with
R](https://learningstatisticswithr.com/book/)*.

### Leaving Feedback

If you like **CGPfunctions**, please consider leaving [feedback
here](https://github.com/ibecav/CGPfunctions/issues).

### Contributing

Contributions in the form of feedback, comments, code, and bug reports
are most welcome. How to contribute:

  - Issues, bug reports, and wish lists: [File a GitHub
    issue](https://github.com/ibecav/CGPfunctions/issues).
  - Contact the maintainer ibecav at gmail.com by email.
