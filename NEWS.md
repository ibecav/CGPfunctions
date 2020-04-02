# CGPfunctions 0.6.0

* Release 0.6.0 in development March 2020
  - Fixed tibble 3.0.0 issue
  - Fixed dependency issues with ggplot2, scales, sjstats and added ggmosaic
  - Adding mosaic plots to PlotXTabs2 still debugging and waiting on the next version
    of ggmosaic for some bu fixes
  - Significant upgrades to seedist including lots of customization options and the
    addition of violin plots.
    
* Release 0.5.9 deployed to CRAN March 2020
  - Added functionality to PlotXTabs2 will be deprecating PlotXtabs next 
    release
  - Another vignette example for newggslopegraph
  - Added chaid_table() including vignette
  - Added cross2_var_vectors function
  - Requiring R >= 3.6.0 to support DescTools
  - Cleaned up dependencies
  - Corrected overly aggresive error checking in newggslopegraph
  - Remove deprecated neweta()
* Release 0.5.8 feature release 20 June 2019
  - Updated newslopegraph to add aditional formatting options
  - Added support for selected themes from ggthemes
  - Updating github web pages
* Release 0.5.7 feature release 12 June 2019
  - Updated newslopegraph to add aditional formatting options
* Release 0.5.4 hotfix release 8 April 2019
  - Updated Plot2WayANOVA to remedy an error when there were zero significant effects
    and post hoc tests failed
* Release 0.5.3 stable release 3 April 2019
  - Updated vignette for Plot2WayANOVA
  - Major improvements and features added to Plot2WayANOVA
* Release 0.5.2 accepted by CRAN 22 March 2019
  - Hotfix to Plot2WayANOVA
  - Minor improvements and features added to Plot2WayANOVA
* v0.5.1 in development on 21 March 2019
  - Minor improvements and features added to Plot2WayANOVA
* Release 0.5.0 released on CRAN 21 March 2019
  - Major improvements and features added to Plot2WayANOVA
  - Deprecated function neweta
* Release 0.4 published on CRAN 13 June 2018
* Release 0.4 published on Github 11 June 2018
  - Changed version # to .4
  - Added new function newggslopegraph 
* Release 0.3.1 (hotfix) published on Github 25 April 2018
  - Changed version # to 3.1
  - Added percent scales to percent plot using the scales package which necessitated changes in DESCRIPTION and NAMESPACE
  - Added some code to keep track of missing values that are omitted in calculations and plotting
  - Creating the ggtitle with a bquote seemed to be creating issues with both performance and also creating intermittent errors if the title was long. Replaced bquote with sprintf and while I was at it changed how it presents
  - Rebuilt vignettes and doco as needed to show the changes

* Release 0.3 published on CRAN

* Release 0.1 
