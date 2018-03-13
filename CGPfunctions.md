CGPfunctions
================
Chuck Powell
2018-03-13

A package that includes functions that I find useful for teaching statistics as well as actually practicing the art. They typically are not "new" methods but rather wrappers around either base R or other packages and concepts I'm trying to master. Currently contains:

-   `Plot2WayANOVA` which as the name implies conducts a 2 way ANOVA and plots the results using `ggplot2`
-   `neweta` which is a helper function that appends the results of a Type II eta squared calculation onto a classic ANOVA table

Vignette Info
-------------

I need more text here.

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
