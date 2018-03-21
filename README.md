CGPfunctions
================
Chuck Powell
2018-03-19


Installation
------------

``` r
# Install from CRAN
install.packages("CGPfunctions")

# Or the development version from GitHub
# install.packages("devtools")
devtools::install_github("ibecav/CGPfunctions")
```

Overview
--------
A package that includes functions that I find useful for teaching statistics as well as actually practicing the art.  They typically are not "new" methods but rather wrappers around either base R or other packages and concepts I'm trying to master.  Currently contains:

- `Plot2WayANOVA` which as the name implies conducts a 2 way ANOVA and plots the results using `ggplot2`
- `neweta` which is a helper function that appends the results of a Type II eta squared calculation onto a classic ANOVA table
- `Mode` which finds the modal value in a vector of data
- `SeeDist` which wraps around ggplot2 to provide visualizations of univariate data.

Vignette Info
-------------

The ANOVA family of statistical techniques that allow us to compare mean differences of one outcome (dependent) variable across two or more groups (levels) of one or independent variables (factor). It is also true that ANOVA is a special case of the GLM or regression models so as the number of levels increase it might make more sense to try one of those approaches.  The 2 Way ANOVA allows for comparisons of mean differences across 2 independent variables `factors` and varying numbers of levels.

The `Plot2WayANOVA` function conducts a classic analysis using existing R functions and packages in a sane and defensible manner not necessarily in the one and only manner.

Imagine that you are interested in understanding whether a car's fuel efficiency (mpg) varies based upon the type of transmission (automatic or manual) and the number of cylinders the engine has. Let's imagine that the `mtcars` data set is actually a random sample of 32 cars from different manufacturers and use the mean `mpg` by `am` and `cyl` to help inform our thinking.  While we expect variation across our sample we're interested in whether the differences between the means by grouping of transmission type and cylinders is significantly different than what we would expect in random variation across the data.  

In simplistic terms we want to know whether `am` matters, `cyl` matters or if it depends on the interaction of the two.  It's this interaction term that typically confuses novices or is difficult to "see".  That's where a good interaction graph can hopefully play a key role, and that's what the `Plot2WayANOVA` focuses on.

There's no lack or tools or capabilities in base R or in the many packages to do this task. What this function tries to do is pull together the disparate pieces with a set of sane defaults and a simple interface to work with it.  At its simplest you would require the library and then enter this command:

`Plot2WayANOVA(mpg~am*cyl, mtcars)` which lays our question out in R's vernacular with a formula and a dataframe.  Optionally we can specify a different confidence level and choose a line or a bar graph.

"Under the hood", however there's a lot of nice features at work.

1. Some basic error checking to ensure a valid formula and dataframe. The function accepts only a fully crossed formula to check for interaction term
2. It ensures the dependent (outcome) variable is numeric and that the two independent (predictor) variables already are or can be coerced to factors – the user is warned on the console if there are problems
3. A check is conducted to see if any of the variables of interest have missing cases – the user is warned on the console if there are problems
4. A summarized table of means, standard deviations, standard errors of the means, confidence intervals, and group sizes for each of the crossed combinations in our example that's 6 groupings 3 levels of cylinder and 2 levels of automatic or manual
5. In addition to the classic ANOVA table eta squared is calculated and appended as an additional column. If you're unfamiliar with them and want to know more especially where the numbers come from I recommend a good introductory stats text.  As noted earlier I recommend *Learning Statistics with R* [LSR](http://dj-navarro.appspot.com/lsr/lsr-0.5.1.pdf) see Table 14-1 on page 432. 
6. The Homogeneity of Variance assumption is tested with Brown-Forsythe
7. The normality assumption is tested with Shapiro-Wilk


Examples
--------

`neweta(aov(mpg~am*cyl, mtcars))`

.

| Source    |   Df|     Sum Sq|     Mean Sq|  F value|      p| sigstars |  eta sq|
|:----------|----:|----------:|-----------:|--------:|------:|:---------|-------:|
| am        |    1|   36.97211|   36.972115|      4.3|  0.048| \*       |   0.033|
| cyl       |    1|  449.53448|  449.534479|     52.0|  0.000| \*\*\*   |   0.399|
| am:cyl    |    1|   29.43939|   29.439389|      3.4|  0.076| .        |   0.026|
| Residuals |   28|  241.92273|    8.640098|       NA|     NA| NA       |   0.215|

`Plot2WayANOVA(mpg~am*cyl, mtcars)`

    #> # A tibble: 4 x 8
    #>   Source       Df `Sum Sq` `Mean Sq` `F value`       p sigstars `eta sq`
    #>   <fct>     <int>    <dbl>     <dbl>     <dbl>   <dbl> <chr>       <dbl>
    #> 1 am            1     36.8     36.8       4.00  0.0560 .          0.0330
    #> 2 cyl           2    456.     228.       24.8   0.     ***        0.405 
    #> 3 am:cyl        2     25.4     12.7       1.40  0.269  ""         0.0230
    #> 4 Residuals    26    239.       9.19     NA    NA      <NA>       0.212 
    #> # A tibble: 6 x 9
    #> # Groups:   am [2]
    #>   am    cyl   TheMean TheSD TheSEM CIMuliplier LowerBound UpperBound     N
    #>   <fct> <fct>   <dbl> <dbl>  <dbl>       <dbl>      <dbl>      <dbl> <int>
    #> 1 0     4        22.9 1.45   0.839        4.30       19.3       26.5     3
    #> 2 0     6        19.1 1.63   0.816        3.18       16.5       21.7     4
    #> 3 0     8        15.0 2.77   0.801        2.20       13.3       16.8    12
    #> 4 1     4        28.1 4.48   1.59         2.36       24.3       31.8     8
    #> 5 1     6        20.6 0.751  0.433        4.30       18.7       22.4     3
    #> 6 1     8        15.4 0.566  0.400       12.7        10.3       20.5     2
    #> Levene's Test for Homogeneity of Variance (center = median)
    #>       Df F value  Pr(>F)  
    #> group  5   2.736 0.04086 *
    #>       26                  
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  MyAOV_residuals
    #> W = 0.96277, p-value = 0.3263

Credits
-------

Many thanks to Dani Navarro and the book &gt; ([Learning Statistics with R](http://www.compcogscisydney.com/learning-statistics-with-r.html)) whose etaSquared function was the genesis of `neweta`.

> "He who gives up \[code\] safety for \[code\] speed deserves neither." ([via](https://twitter.com/hadleywickham/status/504368538874703872))
# README #

This README would cccnormally document whatever steps are necessary to get your application up and running.

### What is this repository for? ###

* Quick summary
* Version
* [Learn Markdown](https://bitbucket.org/tutorials/markdowndemo)

### How do I get set up? ###

* Summary of set up
* Configuration
* Dependencies
* Database configuration
* How to run tests
* Deployment instructions

### Contribution guidelines ###

* Writing tests
* Code review
* Other guidelines

### Who do I talk to? ###

* Repo owner or admin
* Other community or team contact
