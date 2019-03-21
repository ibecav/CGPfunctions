#' CGPfunctions: A package of miscellaneous functions for teaching statistics.
#'
#' A package that includes miscellaneous functions useful for teaching statistics as well as actually practicing the art. They typically are not new methods but rather wrappers around either base R or other packages.
#' 
#' @section Functions included:
#' \itemize{
#'   \item \code{\link{Plot2WayANOVA}} which as the name implies conducts a 2 way ANOVA and plots the results using `ggplot2`
#'   \item \code{\link{PlotXTabs}} Plots cross tabulated variables using `ggplot2`
#'   \item \code{\link{neweta}} which is a helper function that appends the results of a Type II eta squared calculation onto a classic ANOVA table
#'   \item \code{\link{Mode}} which finds the modal value in a vector of data
#'   \item \code{\link{SeeDist}} which wraps around ggplot2 to provide visualizations of univariate data.
#'   \item \code{\link{OurConf}} which wraps around ggplot2 to provide visualizations of sampling confidence intervals.
#' }
#'
#' @docType package
#' @name CGPfunctions
NULL