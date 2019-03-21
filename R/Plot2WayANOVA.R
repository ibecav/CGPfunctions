#' Plot a 2 Way ANOVA using dplyr and ggplot2
#'
#' Takes a formula and a dataframe as input, conducts an analysis of variance
#' using the base R \code{\link[stats]{aov}} function and prints the results
#' (AOV summary table, table of overall model information and
#' table of means) to the console and as a plotted interaction graph (line or
#' bar) using ggplot2.  Also uses Brown-Forsythe test for homogeneity of
#' variance.  Users can also choose to save the plot out as a png file.
#'
#' Details about how the function works in order of steps taken.
#' \enumerate{
#' \item Some basic error checking to ensure a valid formula and dataframe.
#' Only accepts fully *crossed* formula to check for interaction term
#' \item Ensure the dependent (outcome) variable is numeric and that the two
#' independent (predictor) variables are or can be coerced to factors -- user
#' warned on the console
#' \item Remove missing cases -- user warned on the console
#' \item Use \code{dplyr} to calculate a summarized table of means,
#' sds, standard errors of the means, confidence intervals, and group sizes.
#' \item Use the \code{aov} function to execute an Analysis of Variance (ANOVA)
#' \item Use the \code{\link[sjstats]{anova_stats}} function to calculate eta squared values.
#' If the design is unbalanced warn the user and use Type II sums of squares
#' \item Produce a standard ANOVA table with a column for eta-squared appended
#' \item Use the \code{leveneTest} for testing Homogeneity of Variance
#' assumption with Brown-Forsythe
#' \item Use the \code{shapiro.test} for testing normality assumption with Shapiro-Wilk
#' \item Use \code{ggplot2} to plot an interaction plot of the type the user specified }
#'
#' @usage Plot2WayANOVA(formula, dataframe = NULL, confidence=.95,
#'     plottype = "bar", xlab = NULL, ylab = NULL, title = NULL,
#'     subtitle = NULL, interact.line.size = 2, mean.plotting = TRUE,
#'     mean.ci = TRUE, mean.size = 4, mean.color = "darkred",
#'     PlotSave = FALSE)
#' @param formula a valid R formula with a numeric dependent (outcome)
#' variable, and two independent (predictor) variables e.g. \code{mpg~am*vs}.
#' The independent variables are forced to factors (with warning) if possible.
#' @param dataframe a dataframe or an object that can be coerced to a dataframe
#' @param confidence what confidence level for confidence intervals
#' @param plottype bar or line (quoted)
#' @param PlotSave a logical indicating whether the user wants to save the plot
#'  as a png file
#' @param xlab,ylab Labels for `x` and `y` axis variables. If `NULL` (default),
#'   variable names for `x` and `y` will be used.
#' @param title The text for the plot title. A generic default is provided.
#' @param subtitle The text for the plot subtitle. If `NULL` (default), key
#'   model information is provided as a subtitle.
#' @param interact.line.size Line size for the line connecting mean points
#'   (Default: `2`).
#' @param mean.plotting Logical that decides whether mean is to be highlighted
#'   and its value to be displayed (Default: `TRUE`).
#' @param mean.ci Logical that decides whether 95% confidence interval for mean
#'   is to be displayed (Default: `TRUE`).
#' @param mean.color Color for the data point corresponding to mean (Default:
#'   `"darkred"`).
#' @param mean.size Point size for the data point corresponding to mean
#'   (Default: `4`).
#' @return A list with 5 elements which is returned invisibly. The items are always sent
#' to the console for display  The plot is always sent to the default plot device
#' but for user convenience the function also returns a named list with the following items
#' in case the user desires to save them or further process them. \code{$ANOVATable},
#' \code{$ModelSummary}, \code{$MeansTable}, \code{$BFTest}, and \code{$SWTest}.
#'
#' @author Chuck Powell
#' @seealso \code{\link[stats]{aov}}, \code{\link[car]{leveneTest}},
#' \code{\link[sjstats]{anova_stats}}, \code{\link[stats]{replications}},
#' \code{\link[stats]{shapiro.test}}
#' @examples
#' 
#' Plot2WayANOVA(mpg ~ am * cyl, mtcars, plottype = "line")
#' Plot2WayANOVA(mpg ~ am * vs, mtcars, confidence = .99)
#' @importFrom dplyr group_by summarise %>% n
#' @import ggplot2
#' @import rlang
#' @importFrom methods is
#' @importFrom stats anova aov lm pf qt replications sd symnum residuals shapiro.test
#' @importFrom tibble as_tibble
#' @importFrom car leveneTest
#' @importFrom sjstats anova_stats
#' @importFrom broomExtra glance
#' @export
#'
# Stable version 0.2
Plot2WayANOVA <- function(formula,
                          dataframe = NULL,
                          confidence = .95,
                          plottype = "bar",
                          xlab = NULL,
                          ylab = NULL,
                          title = NULL,
                          subtitle = NULL,
                          interact.line.size = 2,
                          mean.plotting = TRUE,
                          mean.ci = TRUE,
                          mean.size = 4,
                          mean.color = "darkred",
                          PlotSave = FALSE) {

  # -------- to appease R CMD Check? ----------------
  TheMean <- NULL
  TheSEM <- NULL
  CIMuliplier <- NULL
  LowerBound <- NULL
  UpperBound <- NULL

  # -------- error checking ----------------
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
  if (length(match.call()) - 1 <= 1) {
    stop("Not enough arguments passed...
         requires at least a formula with a DV and 2 IV plus a dataframe")
  }
  if (missing(formula)) {
    stop("\"formula\" argument is missing, with no default")
  }
  if (!is(formula, "formula")) {
    stop("\"formula\" argument must be a formula")
  }
  if (length(formula) != 3) {
    stop("invalid value for \"formula\" argument")
  }
  vars <- all.vars(formula)
  chkinter <- all.names(formula)
  if (length(vars) != 3) {
    stop("invalid value for \"formula\" argument")
  }
  if ("+" %in% chkinter) {
    stop("Sorry you need to use an asterisk not a plus sign in 
         the formula so the interaction can be plotted")
  }
  if ("~" == chkinter[2]) {
    stop("Sorry you can only have one dependent variable so only 
         one tilde is allowed ~ you have two or more")
  }

  # we can trust the basics grab the variable names from formula
  # these are now of ***class character***
  depvar <- vars[1]
  iv1 <- vars[2]
  iv2 <- vars[3]

  # create a filename in case they want to save png
  potentialfname <- paste0(depvar, "by", iv1, "and", iv2, ".png")

  if (missing(dataframe)) {
    stop("You didn't specify a data frame to use")
  }
  if (!exists(deparse(substitute(dataframe)))) {
    stop("That dataframe does not exist\n")
  }
  if (!is(dataframe, "data.frame")) {
    stop("The dataframe name you specified is not valid\n")
  }
  if (!(depvar %in% names(dataframe))) {
    stop(paste0(
      "'", depvar, "' is not the name of a variable in '",
      deparse(substitute(dataframe)), "'"
    ))
  }
  if (!(iv1 %in% names(dataframe))) {
    stop(paste0(
      "'", iv1, "' is not the name of a variable in '",
      deparse(substitute(dataframe)), "'"
    ))
  }
  if (!(iv2 %in% names(dataframe))) {
    stop(paste0(
      "'", iv2, "' is not the name of a variable in '",
      deparse(substitute(dataframe)), "'"
    ))
  }

  # force it to a data frame
  dataframe <- dataframe[, c(depvar, iv1, iv2)]

  # -------- x & y axis labels ----------------------------

  # if `xlab` is not provided, use the variable `x` name
  if (is.null(xlab)) {
    xlab <- iv1
  }

  # if `ylab` is not provided, use the variable `y` name
  if (is.null(ylab)) {
    ylab <- depvar
  }

  # -------- check variable types ----------------

  if (!is(dataframe[, depvar], "numeric")) {
    stop("dependent variable must be numeric")
  }
  if (!is(dataframe[, iv1], "factor")) {
    message(paste0("\nConverting ", iv1, " to a factor --- check your results"))
    dataframe[, iv1] <- as.factor(dataframe[, iv1])
  }
  if (!is(dataframe[, iv2], "factor")) {
    message(paste0("\nConverting ", iv2, " to a factor --- check your results"))
    dataframe[, iv2] <- as.factor(dataframe[, iv2])
  }

  # grab the names of the factor levels
  factor1.names <- levels(dataframe[, iv1])
  factor2.names <- levels(dataframe[, iv2])

  if (!is(confidence, "numeric") | length(confidence) != 1 |
    confidence < .5 | confidence > .9991) {
    stop("\"confidence\" must be a number between .5 and 1")
  }
  if (plottype != "bar") {
    plottype <- "line"
  }

  # -------- Remove missing cases notify user ----------------

  missing <- apply(is.na(dataframe), 1, any)
  if (any(missing)) {
    warning(paste(sum(missing)), " case(s) removed because of missing data")
  }
  dataframe <- dataframe[!missing, ]

  # -------- Build summary dataframe ----------------

  newdata <- dataframe %>%
    group_by(!!sym(iv1), !!sym(iv2)) %>%
    summarise(
      TheMean = mean(!!sym(depvar), na.rm = TRUE),
      TheSD = sd(!!sym(depvar), na.rm = TRUE),
      TheSEM = sd(!!sym(depvar), na.rm = TRUE) / sqrt(n()),
      CIMuliplier = qt(confidence / 2 + .5, n() - 1),
      LowerBound = TheMean - TheSEM * CIMuliplier,
      UpperBound = TheMean + TheSEM * CIMuliplier,
      N = n()
    )

  # -------- Run tests and procedures ----------------

  # run analysis of variance
  MyAOV <- aov(formula, dataframe)
  # run custom eta squared function
  WithETA <- sjstats::anova_stats(MyAOV)
  # creating model summary dataframe
  model_summary <- broomExtra::glance(MyAOV)
  # Run Brown-Forsythe
  BFTest <- car::leveneTest(MyAOV)
  # Grab the residuals and run Shapiro-Wilk
  MyAOV_residuals <- residuals(object = MyAOV)
  SWTest <- shapiro.test(x = MyAOV_residuals) # run Shapiro-Wilk test

  # -------- save the common plot items as a list to be used ---------

  # Make a default title
  cipercent <- round(confidence * 100, 2)
  # if `title` is not provided, use this generic
  if (is.null(title)) {
    title <- bquote(
      "Group means with" ~ .(cipercent) * "% confidence intervals"
    )
  }

  # compute CI's for R squared using Olkin and Finn's approximation
  denominator <- (nrow(dataframe)^2 - 1) * (3 + nrow(dataframe))
  numerator <- (4 * model_summary$r.squared) * ((1 - model_summary$r.squared)^2) * (nrow(dataframe) - 2 - 1)^2
  ser2 <- sqrt(numerator / denominator)
  tvalue <- qt((1 - confidence) / 2, nrow(dataframe) - 3)
  limit1 <- model_summary$r.squared - tvalue * ser2
  limit2 <- model_summary$r.squared + tvalue * ser2
  ULr2 <- max(limit1, limit2)
  LLr2 <- min(limit1, limit2)

  # make pretty labels
  rsquared <- round(model_summary$r.squared, 3)
  cilower <- round(LLr2, 3)
  ciupper <- round(ULr2, 3)
  AICnumber <- round(model_summary$AIC, 1)
  BICnumber <- round(model_summary$BIC, 1)
  # if `subtitle` is not provided, use this generic
  if (is.null(subtitle)) {
    subtitle <- bquote(
      "R squared =" ~ .(rsquared) * ", CI[" ~ .(cilower) * ", " ~ .(ciupper) * " ], AIC =" ~ .(AICnumber) * ", BIC =" ~ .(BICnumber)
    )
  }

  commonstuff <- list(
    xlab(xlab),
    ylab(ylab),
    scale_colour_hue(l = 40),
    ggtitle(title, subtitle = subtitle)
  )

  # -------- switch for bar versus line plot ---------

  switch(plottype,
    bar =
      p <- newdata %>%
        ggplot(aes_string(
          x = iv1,
          y = "TheMean",
          colour = iv2,
          fill = iv2,
          group = iv2
        )) +
        geom_bar(
          stat = "identity",
          position = "dodge"
        ) +
        geom_errorbar(aes(ymin = LowerBound, ymax = UpperBound),
          width = .5,
          position = position_dodge(0.9),
          show.legend = FALSE
        ) +
        commonstuff,
    line =
      p <- newdata %>%
        ggplot(aes_string(
          x = iv1,
          y = "TheMean",
          colour = iv2,
          fill = iv2,
          group = iv2
        )) +
        geom_point(
          data = dataframe,
          mapping = aes(
            x = !!sym(iv1),
            y = !!sym(depvar)
          ),
          alpha = .4
        ) +
        geom_errorbar(aes(
          ymin = LowerBound,
          ymax = UpperBound
        ),
        width = .2
        ) +
        geom_line(size = interact.line.size) +
        geom_point(aes(y = TheMean),
          shape = 23,
          size = mean.size,
          color = mean.color,
          alpha = 1
        ) +
        geom_violin(
          data = dataframe,
          mapping = aes(
            x = !!sym(iv1),
            y = !!sym(depvar),
            group = !!sym(iv1)
          ),
          width = 0.5,
          alpha = 0.2,
          fill = "white",
          show.legend = FALSE
        ) +
        commonstuff
  )


  # -------- Warn user of unbalanced design ----------------

  if (is.list(replications(formula, dataframe))) {
    message("\nYou have an unbalanced design. Using Type II sum of squares, 
            eta squared may not sum to 1.0 \n")
    print(WithETA)
  }
  else {
    message("\nYou have a balanced design. \n")
    print(WithETA)
  }

  # -------- Print tests and tables ----------------

  message("\nMeasures of overall model fit\n")
  print(model_summary)
  message("\nTable of group means\n")
  print(newdata)
  message("\nTesting Homogeneity of Variance with Brown-Forsythe \n")
  if (BFTest$`Pr(>F)`[[1]] <= .05) {
    message("   *** Possible violation of the assumption ***")
  }
  print(BFTest)
  message("\nTesting Normality Assumption with Shapiro-Wilk \n")
  if (SWTest$p.value <= .05) {
    message("   *** Possible violation of the assumption.  You may 
            want to plot the residuals to see how they vary from normal ***")
  }
  print(SWTest)

  # -------- Print the plot itself ----------------

  message("\nInteraction graph plotted...")
  print(p)


  # -------- Return stuff to user ----------------

  whattoreturn <- list(
    ANOVATable = WithETA,
    ModelSummary = model_summary,
    MeansTable = newdata,
    BFTest = BFTest,
    SWTest = SWTest
  )
  if (PlotSave) {
    ggsave(potentialfname, device = "png")
    whattoreturn <- list(
      ANOVATable = WithETA,
      ModelSummary = model_summary,
      MeansTable = newdata,
      BFTest = BFTest,
      SWTest = SWTest,
      pFileName = potentialfname
    )
  }
  return(invisible(whattoreturn))
}
