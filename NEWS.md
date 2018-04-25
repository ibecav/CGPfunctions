# CGPfunctions 0.3.1


* Release 0.1 
* Release 0.3 published on CRAN
* Release 0.3.1 pubpished on Github

-- Changed version # to 3.1
-- Added percent scales to percent plot using the scales package which necessitated changes in DESCRIPTION and NAMESPACE
-- Added some code to keep track of missing values that are omitted in calculations and plotting
-- Creating the ggtitle with a bquote seemed to be creating issues with both performance and also creating intermittent errors if the title was long. Replaced bquote with sprintf and while I was at it changed how it presents
-- Rebuilt vignettes and doco as needed to show the changes


