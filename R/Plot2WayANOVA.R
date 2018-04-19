#' Plot a 2 Way ANOVA using dplyr and ggplot2
#' 
#' Takes a formula and a dataframe as input, conducts an analysis of variance
#' using the base R aov command and produces the results (AOV summary table and
#' table of means) to the console and as a plotted interaction graph (line or
#' bar) using ggplot2.  Also uses Brown-Forsythe test for homogeneity of
#' variance.
#' 
#' Details about how the function works in order of steps taken. 
#' \enumerate{
#' \item Some basic error checking to ensure a valid formula and dataframe.
#' Only accepts fully crossed formula to check for interaction term 
#' \item Ensure the dependent (outcome) variable is numeric and that the two
#' independent (predictor) variables are or can be coerced to factors -- user
#' warned on the console 
#' \item Remove missing cases -- user warned on the console 
#' \item Use \code{dplyr} to calculate a summarized table of means,
#' sds, standard errors of the means, confidence intervals, and group sizes.
#' \item Use the \code{aov} function to execute an Analysis of Variance (ANOVA)
#' \item Use the \code{\link{neweta}} function to calculate eta squared values.
#' If the design is unbalanced warn the user and use Type II sums of squares
#' \item Produce a standard ANOVA table with a column for eta-squared appended
#' \item Use the \code{leveneTest} for testing Homogeneity of Variance
#' assumption with Brown-Forsythe 
#' \item Use the \code{shapiro.test} for testing normality assumption with Shapiro-Wilk 
#' \item Use \code{ggplot2} to plot an interaction plot of the type the user specified }
#' 
#' @usage Plot2WayANOVA(formula, dataframe = NULL, confidence=.95, 
#'     plottype = "bar", PlotSave = FALSE)
#' @param formula a valid R formula with a numeric dependent (outcome)
#' variable, and two independent (predictor) variables e.g. \code{mpg~am*vs}.
#' The independent variables are forced to factors (with warning) if possible.
#' @param dataframe a dataframe or an object that can be coerced to a dataframe
#' @param confidence what confidence level for confidence intervals
#' @param plottype bar or line (quoted)
#' @param PlotSave a logical indicating whether the user wants to save the plot as a png file
#' @return A list with 4 elements which is returned invisibly. The items are always sent 
#' to the console for display  The plot is always sent to the default plot device
#' but for user convenience the function also returns a named list with the following items
#' in case the user desires to save them or further process them. \code{$ANOVATable}, 
#' \code{$MeansTable}, \code{$BFTest}, and \code{$SWTest}. 
#'
#' @author Chuck Powell
#' @seealso \code{\link[stats]{aov}}, \code{\link[car]{leveneTest}},
#' \code{\link{neweta}}, \code{\link[stats]{replications}},
#' \code{\link[stats]{shapiro.test}}
#' @examples
#' 
#' Plot2WayANOVA(mpg~am*cyl, mtcars, plottype = "line")
#' Plot2WayANOVA(mpg~am*vs, mtcars, confidence = .99)
#' 
#' @importFrom dplyr group_by summarise %>% n
#' @import ggplot2
#' @import rlang
#' @importFrom methods is
#' @importFrom stats anova aov lm pf qt replications sd symnum residuals shapiro.test
#' @importFrom tibble as_tibble
#' @importFrom car leveneTest
#' @export
#' 
# Stable version 0.2
Plot2WayANOVA <- function(formula, dataframe = NULL, confidence=.95, plottype = "bar", PlotSave = FALSE)
{
  # to appease R CMD Check?
  TheMean <- NULL
  TheSEM <- NULL
  CIMuliplier <- NULL
  LowerBound <- NULL
  UpperBound <- NULL
  # error checking
  if (!requireNamespace("ggplot2")) {
    stop("Can't continue can't load ggplot2")
  }
  theme_set(theme_bw())
  if (!requireNamespace("dplyr")) {
    stop("Can't continue can't load dplyr")
  }
  if (!requireNamespace("rlang")) {
    stop("Can't continue can't load rlang")
  }
  if (length(match.call())-1 <= 1) {
    stop("Not enough arguments passed... requires at least a formula with a DV and 2 IV plus a dataframe")
  }
  if (missing(formula)) {
    stop("\"formula\" argument is missing, with no default")
  }
  if (!is(formula, "formula")) {
    stop("\"formula\" argument must be a formula")
  }
  if (length(formula) != 3)
    stop("invalid value for \"formula\" argument")
  vars <- all.vars(formula)
  chkinter <- all.names(formula)
  if (length(vars) != 3)
    stop("invalid value for \"formula\" argument")
  if ("+" %in% chkinter)
    stop("Sorry you need to use an asterisk not a plus sign in the formula so the interaction can be plotted")
  if ("~" == chkinter[2])
    stop("Sorry you can only have one dependent variable so only one ~ you have two or more")
  depvar <- vars[1]
  iv1 <- vars[2]
  iv2 <- vars[3]
  potentialfname <- paste0(depvar,"by",iv1,"and",iv2,".png")
  if (missing(dataframe))
    stop("You didn't specify a data frame to use")
  if (!exists(deparse(substitute(dataframe))))
    stop("That dataframe does not exist\n")
  if (!is(dataframe, "data.frame"))
    stop("The dataframe name you specified is not valid\n")
  if (!(depvar %in% names(dataframe))) {
    stop(paste0("'", depvar, "' is not the name of a variable in '", deparse(substitute(dataframe)), "'"))
    }
  if (!(iv1 %in% names(dataframe))) {
      stop(paste0("'", iv1, "' is not the name of a variable in '", deparse(substitute(dataframe)), "'"))
    }
  if (!(iv2 %in% names(dataframe))) {
      stop(paste0("'", iv2, "' is not the name of a variable in '", deparse(substitute(dataframe)), "'"))
    }
# force it to a data frame 
  dataframe <- dataframe[, c(depvar, iv1, iv2)]
  if (!is(dataframe[, depvar], "numeric"))
    stop("dependent variable must be numeric")
  if (!is(dataframe[, iv1], "factor")) {
    message(paste0("\nConverting ", iv1, " to a factor --- check your results"))
    dataframe[, iv1] <- as.factor(dataframe[, iv1])
  }
  if (!is(dataframe[, iv2], "factor")) {
    message(paste0("\nConverting ", iv2, " to a factor --- check your results"))
    dataframe[, iv2] <- as.factor(dataframe[, iv2])
  }
  factor1.names <- levels(dataframe[, iv1])
  factor2.names <- levels(dataframe[, iv2])
  if (!is(confidence, "numeric") | length(confidence) != 1 |
      confidence < .5 | confidence > .9991) {
    stop("\"confidence\" must be a number between .5 and 1")
  }
  if (plottype != "bar" ) {
    plottype <- "line"
  }
  missing <- apply(is.na(dataframe), 1, any)
  if (any(missing))
    warning(paste(sum(missing)), " case(s) removed because of missing data")
  dataframe <- dataframe[!missing, ]

  dataframe %>%
    group_by(!!sym(iv1),!!sym(iv2)) %>%
    summarise(TheMean = mean(!!sym(depvar),na.rm=TRUE),
              TheSD = sd(!!sym(depvar),na.rm=TRUE),
              TheSEM = sd(!!sym(depvar),na.rm=TRUE)/sqrt(n()),
              CIMuliplier = qt(confidence/2 + .5, n()-1),
              LowerBound = TheMean-TheSEM*CIMuliplier,
              UpperBound = TheMean+TheSEM*CIMuliplier,
              N = n()) -> newdata

  MyAOV <- aov(formula, dataframe)
  WithETA <- neweta(MyAOV)
  BFTest <- car::leveneTest(MyAOV)
  MyAOV_residuals <- residuals( object = MyAOV )
  SWTest <- shapiro.test( x = MyAOV_residuals ) # run Shapiro-Wilk test

# save the plot common items as a list to be used
  cipercent <- round(confidence*100,2)
  commonstuff <- list(
    xlab(iv1),
    ylab(depvar),
    scale_colour_hue( l=40),
    ggtitle(bquote("Group means with"~.(cipercent)*"% confidence intervals"))
  )

  switch(plottype,
         bar =
           newdata %>% ggplot(aes_string(x = iv1, y= "TheMean", colour= iv2, fill= iv2, group=iv2)) +
            geom_bar(stat = "identity", position = "dodge") +
            geom_errorbar(aes(ymin=LowerBound, ymax=UpperBound), width=.5, position = "dodge", show.legend = FALSE) +
            commonstuff -> p,
         line =
           newdata %>% ggplot(aes_string(x = iv1, y= "TheMean", colour= iv2, fill= iv2, group=iv2)) +
            geom_errorbar(aes(ymin=LowerBound, ymax=UpperBound), width=.2) +
            geom_line() +
            geom_point(aes(y=TheMean)) +
            commonstuff -> p
  )
  
  if (is.list(replications(formula, dataframe))) {
    message("\nYou have an unbalanced design. Using Type II sum of squares, eta squared may not sum to 1.0 \n")
    print(WithETA)
  }
  else {
    message("\nYou have a balanced design. \n")
    print(WithETA)
  }

  message("\nTable of group means\n")
  print(newdata)
  message("\nTesting Homogeneity of Variance with Brown-Forsythe \n")
  if (BFTest$`Pr(>F)`[[1]] <= .05) {
    message("   *** Possible violation of the assumption ***")
  }
  print(BFTest)
  message("\nTesting Normality Assumption with Shapiro-Wilk \n")
  if (SWTest$p.value <= .05) {
    message("   *** Possible violation of the assumption.  You may want to plot the residuals to see how they vary from normal ***")
  }
  print(SWTest)
  message("\nInteraction graph plotted...")
  print(p)
  whattoreturn <- list(ANOVATable = WithETA, MeansTable = newdata, BFTest = BFTest, SWTest = SWTest)
  if (PlotSave) {
    ggsave(potentialfname,device = "png")
    whattoreturn <- list(ANOVATable = WithETA, MeansTable = newdata, BFTest = BFTest, SWTest = SWTest, pFileName = potentialfname)
  }
  return(invisible(whattoreturn))
}


