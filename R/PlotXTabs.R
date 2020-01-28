#' Plot a Cross Tabulation of two variables using dplyr and ggplot2
#'
#' Takes a dataframe and at least two variables as input, conducts a
#' crosstabulation of the variables using dplyr. Removes NAs and then
#' plots the results as one of three types of bar (column) graphs
#' using ggplot2.  The function accepts either bare variable names or
#' column numbers as input (see examples for the possibilities)
#'
#' @usage PlotXTabs(dataframe, xwhich, ywhich, plottype = "side")
#' @param dataframe an object that is of class dataframe
#' @param xwhich either a bare variable name that is valid in the
#' dataframe or one or more column numbers. An attempt will be
#' made to coerce the variable to a factor but odd plots will occur
#' if you pass it a variable that is by rights continuous in nature.
#' @param ywhich either a bare variable name that is valid in the
#' dataframe or one or more column numbers that exist in the dataframe.
#' An attempt will be
#' made to coerce the variable to a factor but odd plots will occur
#' if you pass it a variable that is by rights continuous in nature.
#' @param plottype one of three options "side", "stack" or "percent"
#'
#' @return One or more ggplots to the default graphics device as well as
#' advisory information in the console
#' @export
#' @import ggplot2 scales
#' @importFrom dplyr group_by summarise %>% count filter mutate
#'
#' @author Chuck Powell
#' @seealso \code{\link[janitor]{janitor}}
#'
#' @examples
#' PlotXTabs(mtcars, am, vs)
#' PlotXTabs(mtcars, am, vs, "stack")
#' PlotXTabs(mtcars, am, vs, "percent")
#' PlotXTabs(mtcars, am, 8, "side")
#' PlotXTabs(mtcars, 8, am, "stack")
#' PlotXTabs(mtcars, am, c(8, 10), "percent")
#' PlotXTabs(mtcars, c(10, 8), am)
#' PlotXTabs(mtcars, c(2, 9), c(10, 8), "mispelled")
#' \dontrun{
#' PlotXTabs(happy, happy, sex) # baseline
#' PlotXTabs(happy, 2, 5, "stack") # same thing using column numbers
#' PlotXTabs(happy, 2, c(5:9), plottype = "percent") # multiple columns RHS
#' PlotXTabs(happy, c(2, 5), 9, plottype = "side") # multiple columns LHS
#' PlotXTabs(happy, c(2, 5), c(6:9), plottype = "percent")
#' PlotXTabs(happy, happy, c(6, 7, 9), plottype = "percent")
#' PlotXTabs(happy, c(6, 7, 9), happy, plottype = "percent")
#' }
#'
PlotXTabs <- function(dataframe, xwhich, ywhich, plottype = "side") {
  theme_set(theme_bw())
  if (length(match.call()) <= 3) {
    stop("Not enough arguments passed... requires a dataframe, plus at least two variables")
  }
  argList <- as.list(match.call()[-1])
  if (!exists(deparse(substitute(dataframe)))) {
    stop("The first object in your list does not exist. It should be a dataframe")
  }
  if (!is(dataframe, "data.frame")) {
    stop("The first name you passed does not appear to be a data frame")
  }
  # process plottype logic -- default is side anything mispelled or not listed is also side
  switch(plottype,
    side = list(
      geom_bar(position = "dodge", stat = "identity"),
      ylab("Count")
    ) -> whichbar,
    stack = list(
      geom_bar(stat = "identity"),
      ylab("Count")
    ) -> whichbar,
    percent = list(
      geom_bar(stat = "identity", position = "fill"),
      ylab("Percent"),
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.10))
    ) -> whichbar,
    list(
      geom_bar(position = "dodge", stat = "identity"),
      ylab("Count")
    ) -> whichbar
  )

  PlotMagic <- function(dataframe, aaa, bbb, whichbar, dfname, xname, yname) {
    dataframe %>%
      filter(!is.na(!!aaa), !is.na(!!bbb)) %>%
      mutate(!!quo_name(aaa) := factor(!!aaa), !!quo_name(bbb) := factor(!!bbb)) %>%
      group_by(!!aaa, !!bbb) %>%
      count() -> tempdf
    originalcount <- nrow(dataframe)
    newcount <- sum(tempdf$n)
    missingcount <- originalcount - newcount
    tempdf %>%
      ggplot(aes_(fill = aaa, y = ~n, x = bbb)) +
      whichbar +
      ggtitle(sprintf("Crosstab of %s N = %i after removing %i missing cases", dfname, newcount, missingcount),
        subtitle = sprintf("Variables %s by %s ", yname, xname)
      ) -> p
    print(p)
  }

  # If both are bare variables and found in the dataframe immediately print the plot
  if (deparse(substitute(xwhich)) %in% names(dataframe) & deparse(substitute(ywhich)) %in% names(dataframe)) { # both are names in the dataframe
    aaa <- enquo(xwhich)
    bbb <- enquo(ywhich)
    xname <- deparse(substitute(xwhich))
    yname <- deparse(substitute(ywhich))
    dfname <- deparse(substitute(dataframe))
    PlotMagic(dataframe, aaa, bbb, whichbar, dfname, xname, yname)
    return(message(paste("Plotted dataset", argList$dataframe, "variables", argList$xwhich, "by", argList$ywhich)))
  } else { # is at least one in the dataframe?
    # Is at least one of them a bare variable in the dataframe
    if (deparse(substitute(xwhich)) %in% names(dataframe)) { # xwhich is in the dataframe
      aaa <- enquo(xwhich)
      if (class(try(eval(ywhich))) %in% c("integer", "numeric")) { # ywhich is column numbers
        indvars <- vector("list", length = length(ywhich))
        totalcombos <- 1 # keep track of where we are
        xname <- deparse(substitute(xwhich))
        dfname <- deparse(substitute(dataframe))
        message("Creating the variable pairings from dataframe ", dfname)
        for (k in seq_along(ywhich)) { # for loop
          indvarsbare <- as.name(colnames(dataframe[ywhich[[k]]]))
          cat("Plot #", totalcombos, " ", xname,
            " with ", as.name(colnames(dataframe[ywhich[[k]]])), "\n",
            sep = ""
          )
          bbb <- enquo(indvarsbare)
          yname <- deparse(substitute(indvarsbare))
          PlotMagic(dataframe, aaa, bbb, whichbar, dfname, xname, yname)
          totalcombos <- totalcombos + 1
        } # end of for loop
        return(message("Plotting complete"))
      } else { # ywhich is NOT suitable
        stop("Sorry I don't understand your ywhich variable(s)")
      } #
    } else { # xwhich wasn't try ywhich
      if (deparse(substitute(ywhich)) %in% names(dataframe)) { # yes ywhich is
        bbb <- enquo(ywhich)
        if (class(try(eval(xwhich))) %in% c("integer", "numeric")) { # then xwhich a suitable number
          # Build one list two ways
          depvars <- vector("list", length = length(xwhich))
          totalcombos <- 1 # keep track of where we are
          yname <- deparse(substitute(ywhich))
          dfname <- deparse(substitute(dataframe))
          message("Creating the variable pairings from dataframe ", dfname)
          for (j in seq_along(xwhich)) {
            depvarsbare <- as.name(colnames(dataframe[xwhich[[j]]]))
            cat("Plot #", totalcombos, " ", as.name(colnames(dataframe[xwhich[[j]]])),
              " with ", yname, "\n",
              sep = ""
            )
            aaa <- enquo(depvarsbare)
            xname <- deparse(substitute(depvarsbare))
            PlotMagic(dataframe, aaa, bbb, whichbar, dfname, xname, yname)
            totalcombos <- totalcombos + 1
          } # end of for loop
          return(message("Plotting complete"))
        } else { # xwhich is NOT suitable
          stop("Sorry I don't understand your xwhich variable(s)")
        } # end of else because xwhich not suitable
      } # end of if
    }
  }

  # If both variables are numeric print the plot(s)
  if (class(try(eval(xwhich))) %in% c("integer", "numeric") & class(try(eval(ywhich))) %in% c("integer", "numeric")) {
    indvars <- vector("list", length = length(ywhich))
    depvars <- vector("list", length = length(xwhich))
    dfname <- deparse(substitute(dataframe))
    totalcombos <- 1 # keep track of where we are
    message("Creating the variable pairings from dataframe ", dfname)
    for (j in seq_along(xwhich)) {
      for (k in seq_along(ywhich)) {
        depvarsbare <- as.name(colnames(dataframe[xwhich[[j]]]))
        indvarsbare <- as.name(colnames(dataframe[ywhich[[k]]]))
        cat("Plot #", totalcombos, " ", as.name(colnames(dataframe[xwhich[[j]]])),
          " with ", as.name(colnames(dataframe[ywhich[[k]]])), "\n",
          sep = ""
        )
        aaa <- enquo(depvarsbare)
        bbb <- enquo(indvarsbare)
        xname <- deparse(substitute(depvarsbare))
        yname <- deparse(substitute(indvarsbare))
        PlotMagic(dataframe, aaa, bbb, whichbar, dfname, xname, yname)
        totalcombos <- totalcombos + 1
      } # end of inner for loop
    } # end of outer for loop
    return(message("Plotting complete"))
  } # end of if case where all are numeric
} # end of function
# PackageList <- .packages(all.available = TRUE)
# if ("productplots" %in% PackageList) {
#  data("happy",package = "productplots")
# } else {
#  stop("Can't load productplots can't use the following examples")
# }
#' \dontrun{
#' PlotXTabs(happy,happy,sex) # baseline
#' PlotXTabs(happy,2,5,"stack") # same thing using column numbers
#' PlotXTabs(happy, 2, c(5:9), plottype = "percent") # multiple columns RHS
#' PlotXTabs(happy, c(2,5), 9, plottype = "side") # multiple columns LHS
#' PlotXTabs(happy, c(2,5), c(6:9), plottype = "percent")
#' PlotXTabs(happy, happy, c(6,7,9), plottype = "percent")
#' PlotXTabs(happy, c(6,7,9), happy, plottype = "percent")
#' }
