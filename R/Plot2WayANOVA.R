# Formula version 1.0
Plot2WayANOVAf <- function(formula, dataframe = NULL, confidence=.95, plottype = "bar")
{
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
#  if (!exists("neweta")) {
#    if (file.exists("moreate.R")) {
#      message("\nLoading the neweta function \n")
#      source("moreate.R")}
#    else {
#      stop("Can't continue until you load the neweta function")
#    }
#  }
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
    stop("Sorry you need to use an asterisk not a plus sign in the formula")
  depvar <- vars[1]
  iv1 <- vars[2]
  iv2 <- vars[3]
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
  message("\nInteraction graph plotted...")
  return(p)
  #  return(as.data.frame(newdata))
}


