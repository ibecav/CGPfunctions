#' Plot a Slopegraph a la Tufte using dplyr and ggplot2
#'
#' Creates a "slopegraph" as conceptualized by Edward Tufte. Slopegraphs are minimalist
#' and efficient presentations of your data that can simultaneously convey the relative rankings,
#' the actual numeric values, and the changes and directionality of the data over time.
#' Takes a dataframe as input, with three named columns being used to draw the plot.
#' Makes the required adjustments to the ggplot2 parameters and returns the plot.
#'
#' @param dataframe a dataframe or an object that can be coerced to a dataframe.
#' Basic error checking is performed, to include ensuring that the named columns
#' exist in the dataframe. See the \code{\link{newcancer}} dataset for an example of
#' how the dataframe should be organized.
#' @param Times a column inside the dataframe that will be plotted on the x axis.
#' Traditionally this is some measure of time.  The function accepts a column of class
#' ordered, factor or character.  NOTE if your variable is currently a "date" class
#' you must convert before using the function with \code{as.character(variablename)}.
#' @param Measurement a column inside the dataframe that will be plotted on the y axis.
#' Traditionally this is some measure such as a percentage.  Currently the function
#' accepts a column of type integer or numeric.  The slopegraph will be most effective
#' when the measurements are not too disparate.
#' @param Grouping a column inside the dataframe that will be used to group and
#' distinguish measurements.
#' @param Title Optionally the title to be displayed. Title = NULL will remove it
#' entirely. Title = "" will provide an empty title but retain the spacing.
#' @param SubTitle Optionally the sub-title to be displayed.  SubTitle = NULL
#' will remove it entirely. SubTitle = "" will provide and empty title but retain
#' the spacing.
#' @param Caption Optionally the caption to be displayed. Caption = NULL will remove
#' it entirely. Caption = "" will provide and empty title but retain the spacing.
#' @param XTextSize Optionally the font size for the X axis labels to be displayed. XTextSize = 12 is the default must be a numeric. Note that X & Y axis text are on different scales
#' @param YTextSize Optionally the font size for the Y axis labels to be displayed.
#' YTextSize = 3 is the default must be a numeric. Note that X & Y axis text are on
#' different scales
#' @param TitleTextSize Optionally the font size for the Title to be displayed.
#' TitleTextSize = 14 is the default must be a numeric.
#' @param SubTitleTextSize Optionally the font size for the SubTitle to be displayed.
#' SubTitleTextSize = 10 is the default must be a numeric.
#' @param CaptionTextSize Optionally the font size for the Caption to be displayed.
#' CaptionTextSize = 8 is the default must be a numeric.
#' @param TitleJustify Justification of title can be either a character "L",
#'   "R" or "C" or use the \code{hjust = } notation from \code{ggplot2} with
#'   a numeric value between `0` (left) and `1` (right).
#' @param SubTitleJustify Justification of subtitle can be either a character "L",
#'   "R" or "C" or use the \code{hjust = } notation from \code{ggplot2} with
#'   a numeric value between `0` (left) and `1` (right).
#' @param CaptionJustify Justification of caption can be either a character "L",
#'   "R" or "C" or use the \code{hjust = } notation from \code{ggplot2} with
#'   a numeric value between `0` (left) and `1` (right).
#' @param LineThickness Optionally the thickness of the plotted lines that
#' connect the data points. LineThickness = 1 is the default must be a numeric.
#' @param DataTextSize Optionally the font size of the plotted data points. DataTextSize = 2.5
#' is the default must be a numeric.
#' @param DataTextColor Optionally the font color of the plotted data points. `"black"`
#' is the default can be either `colors()` or hex value e.g. "#FF00FF".
#' @param DataLabelPadding Optionally the amount of space between the plotted
#'   data point numbers and the label "box". By default very small = 0.05 to
#'   avoid overlap. Must be a numeric. Too large a value will risk "hiding"
#'   datapoints.
#' @param DataLabelLineSize Optionally how wide a line to plot around the data
#'   label box. By default = 0 to have no visible border line around the
#'   label. Must be a numeric.
#' @param DataLabelFillColor Optionally the fill color or background of the
#'   plotted data points. `"white"` is the default can be any of the `colors()`
#'   or hex value e.g. "#FF00FF".
#' @param LineColor Optionally the color of the plotted lines. By default it will use
#' the ggplot2 color palette for coloring by \code{Grouping}. The user may override
#' with \bold{one} valid color of their choice e.g. "black" (see colors() for choices)
#' \bold{OR}
#' they may provide a vector of colors such as c("gray", "red", "green", "gray", "blue")
#' \bold{OR} a named vector like c("Green" = "gray", "Liberal" = "red", "NDP" = "green",
#' "Others" = "gray", "PC" = "blue"). Any input must be character, and the length
#' of a vector \bold{should} equal the number of levels in \code{Grouping}. If the
#' user does not provide enough colors they will be recycled.
#' @param WiderLabels logical, set this value to \code{TRUE} if your "labels" or
#' \code{Grouping} variable values tend to be long as they are in the \code{newcancer}
#' dataset.  This setting will give them more room in the same plot size.
#' @param ReverseYAxis logical, set this value to \code{TRUE} if you want
#' to reverse the Y scale, especially useful for rankings when you want #1 on
#' top.
#' @param ReverseXAxis logical, set this value to \code{TRUE} if you want
#' to reverse the **factor levels** on the X scale.
#' @param RemoveMissing logical, by default set to \code{TRUE} so that if any \code{Measurement}
#' is missing \bold{all rows} for that \code{Grouping} are removed. If set to \code{FALSE} then
#' the function will try to remove and graph what data it does have. \bold{N.B.} missing values
#' for \code{Times} and \code{Grouping} are never permitted and will generate a fatal error with
#' a warning.
#' @param ThemeChoice character, by default set to \bold{"bw"} the other
#' choices are \bold{"ipsum"}, \bold{"econ"}, \bold{"wsj"}, \bold{"gdocs"},
#' and \bold{"tufte"}.
#'
#'
#' @return a plot of type ggplot to the default plot device
#' @export
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by summarise %>% n case_when
#' @importFrom ggrepel geom_text_repel geom_label_repel
#' @importFrom forcats fct_rev
#' @importFrom methods hasArg
#'
#' @author Chuck Powell
#' @seealso \code{\link{newcancer}} and  \code{\link{newgdp}}
#' @references Based on: Edward Tufte, Beautiful Evidence (2006), pages 174-176.
#' @examples
#' # the minimum command to generate a plot
#' newggslopegraph(newcancer, Year, Survival, Type)
#'
#' # adding a title which is always recommended
#' newggslopegraph(newcancer, Year, Survival, Type,
#'   Title = "Estimates of Percent Survival Rates",
#'   SubTitle = NULL,
#'   Caption = NULL
#' )
#'
#' # simple formatting changes
#' newggslopegraph(newcancer, Year, Survival, Type,
#'   Title = "Estimates of Percent Survival Rates",
#'   LineColor = "darkgray",
#'   LineThickness = .5,
#'   SubTitle = NULL,
#'   Caption = NULL
#' )
#'
#' # complex formatting with recycling and wider labels see vignette for more examples
#' newggslopegraph(newcancer, Year, Survival, Type,
#'   Title = "Estimates of Percent Survival Rates",
#'   SubTitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
#'   Caption = "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk",
#'   LineColor = c("black", "red", "grey"),
#'   LineThickness = .5,
#'   WiderLabels = TRUE
#' )
#'
#' # not a great example but demonstrating functionality
#' newgdp$rGDP <- round(newgdp$GDP)
#'
#' newggslopegraph(newgdp,
#'   Year,
#'   rGDP,
#'   Country,
#'   LineColor = c(rep("grey", 3), "red", rep("grey", 11)),
#'   DataTextSize = 3,
#'   DataLabelFillColor = "gray",
#'   DataLabelPadding = .2,
#'   DataLabelLineSize = .5
#' )
newggslopegraph <- function(dataframe, Times, Measurement, Grouping,
                            Title = "No title given",
                            SubTitle = "No subtitle given",
                            Caption = "No caption given",
                            XTextSize = 12,
                            YTextSize = 3,
                            TitleTextSize = 14,
                            SubTitleTextSize = 10,
                            CaptionTextSize = 8,
                            TitleJustify = "left",
                            SubTitleJustify = "left",
                            CaptionJustify = "right",
                            LineThickness = 1,
                            LineColor = "ByGroup",
                            DataTextSize = 2.5,
                            DataTextColor = "black",
                            DataLabelPadding = 0.05,
                            DataLabelLineSize = 0,
                            DataLabelFillColor = "white",
                            WiderLabels = FALSE,
                            ReverseYAxis = FALSE,
                            ReverseXAxis = FALSE,
                            RemoveMissing = TRUE,
                            ThemeChoice = "bw") {

  # ---------------- theme selection ----------------------------

  if (ThemeChoice == "bw") {
    theme_set(theme_bw())
  } else if (ThemeChoice == "ipsum") {
    theme_set(hrbrthemes::theme_ipsum_rc())
  } else if (ThemeChoice == "econ") {
    theme_set(ggthemes::theme_economist()) ## background = "#d5e4eb"
    if (DataLabelFillColor == "white") {
      DataLabelFillColor <- "#d5e4eb"
    }
  } else if (ThemeChoice == "wsj") {
    theme_set(ggthemes::theme_wsj()) ## background = "#f8f2e4"
    if (DataLabelFillColor == "white") {
      DataLabelFillColor <- "#f8f2e4"
    }
    TitleTextSize <- TitleTextSize - 1
    SubTitleTextSize <- SubTitleTextSize + 1
  } else if (ThemeChoice == "gdocs") {
    theme_set(ggthemes::theme_gdocs())
  } else if (ThemeChoice == "tufte") {
    theme_set(ggthemes::theme_tufte())
  } else {
    theme_set(theme_bw())
  }

  # ---------------- ggplot setup work ----------------------------

  # Since ggplot2 objects are just regular R objects, put them in a list
  MySpecial <- list(
    # Format tweaks
    scale_x_discrete(position = "top"), # move the x axis labels up top
    theme(legend.position = "none"), # Remove the legend
    theme(panel.border = element_blank()), # Remove the panel border
    theme(axis.title.y = element_blank()), # Remove just about everything from the y axis
    theme(axis.text.y = element_blank()),
    theme(panel.grid.major.y = element_blank()),
    theme(panel.grid.minor.y = element_blank()),
    theme(axis.title.x = element_blank()), # Remove a few things from the x axis
    theme(panel.grid.major.x = element_blank()),
    theme(axis.text.x.top = element_text(size = XTextSize, face = "bold")), # and increase font size
    theme(axis.ticks = element_blank()), # Remove x & y tick marks
    theme(plot.title = element_text(
      size = TitleTextSize,
      face = "bold",
      hjust = justifyme(TitleJustify)
    )),
    theme(plot.subtitle = element_text(
      size = SubTitleTextSize,
      hjust = justifyme(SubTitleJustify)
    )),
    theme(plot.caption = element_text(
      size = CaptionTextSize,
      hjust = justifyme(CaptionJustify)
    ))
  )

  # ---------------- input checking ----------------------------

  NTimes <- deparse(substitute(Times)) # name of Times variable
  NMeasurement <- deparse(substitute(Measurement)) # name of Measurement variable
  NGrouping <- deparse(substitute(Grouping)) # name of Grouping variable

  # error checking and setup
  if (length(match.call()) <= 4) {
    stop("Not enough arguments passed requires a dataframe, plus at least three variables")
  }
  argList <- as.list(match.call()[-1])
  if (!hasArg(dataframe)) {
    stop("You didn't specify a dataframe to use", call. = FALSE)
  }
  Ndataframe <- argList$dataframe # name of dataframe
  if (!is(dataframe, "data.frame")) {
    stop(paste0("'", Ndataframe, "' does not appear to be a data frame"))
  }

  if (!NTimes %in% names(dataframe)) {
    stop(paste0("'", NTimes, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (anyNA(dataframe[[NTimes]])) {
    stop(paste0("'", NTimes, "' can not have missing data please remove those rows!"), call. = FALSE)
  }
  if (!NMeasurement %in% names(dataframe)) {
    stop(paste0("'", NMeasurement, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (!NGrouping %in% names(dataframe)) {
    stop(paste0("'", NGrouping, "' is not the name of a variable in the dataframe"), call. = FALSE)
  }
  if (anyNA(dataframe[[NGrouping]])) {
    stop(paste0("'", NGrouping, "' can not have missing data please remove those rows!"), call. = FALSE)
  }
  if (!class(dataframe[[NMeasurement]]) %in% c("integer", "numeric")) {
    stop(paste0("Sorry I need the measured variable '", NMeasurement, "' to be a number"), call. = FALSE)
  }
  if (!"ordered" %in% class(dataframe[[NTimes]])) { # keep checking
    if (!"character" %in% class(dataframe[[NTimes]])) { # keep checking
      if ("factor" %in% class(dataframe[[NTimes]])) { # impose order
        message(paste0("\nConverting '", NTimes, "' to an ordered factor\n"))
        dataframe[[NTimes]] <- factor(dataframe[[NTimes]], ordered = TRUE)
      } else {
        stop(paste0("Sorry I need the variable '", NTimes, "' to be of class character, factor or ordered"), call. = FALSE)
      }
    }
  }

  # ---------------- handle some special options ----------------------------

  if (ReverseXAxis) {
    dataframe[[NTimes]] <- forcats::fct_rev(dataframe[[NTimes]])
  }

  NumbOfLevels <- nlevels(factor(dataframe[[NTimes]]))
  if (WiderLabels) {
    MySpecial <- c(MySpecial, expand_limits(x = c(0, NumbOfLevels + 1)))
  }

  if (ReverseYAxis) {
    MySpecial <- c(MySpecial, scale_y_reverse())
  }

  Times <- enquo(Times)
  Measurement <- enquo(Measurement)
  Grouping <- enquo(Grouping)

  if (length(LineColor) > 1) {
    if (length(LineColor) < length(unique(dataframe[[NGrouping]]))) {
      message(paste0("\nYou gave me ", length(LineColor), " colors I'm recycling colors because you have ", length(unique(dataframe[[NGrouping]])), " ", NGrouping, "s\n"))
      LineColor <- rep(LineColor, length.out = length(unique(dataframe[[NGrouping]])))
    }
    LineGeom <- list(geom_line(aes_(color = Grouping), size = LineThickness), scale_color_manual(values = LineColor))
  } else {
    if (LineColor == "ByGroup") {
      LineGeom <- list(geom_line(aes_(color = Grouping, alpha = 1), size = LineThickness))
    } else {
      LineGeom <- list(geom_line(aes_(), size = LineThickness, color = LineColor))
    }
  }

  # complex logic to sort out missing values if any
  if (anyNA(dataframe[[NMeasurement]])) { # are there any missing
    if (RemoveMissing) { # which way should we handle them
      dataframe <- dataframe %>%
        group_by(!!Grouping) %>%
        filter(!anyNA(!!Measurement)) %>%
        droplevels()
    } else {
      dataframe <- dataframe %>%
        filter(!is.na(!!Measurement))
    }
  }

  # ---------------- main ggplot routine ----------------------------

  dataframe %>%
    ggplot(aes_(group = Grouping, y = Measurement, x = Times)) +
    LineGeom +
    # left side y axis labels
    geom_text_repel(
      data = . %>% filter(!!Times == min(!!Times)),
      aes_(label = Grouping),
      hjust = "left",
      box.padding = 0.10,
      segment.color = "gray",
      segment.alpha = 0.6,
      fontface = "bold",
      size = YTextSize,
      nudge_x = -1.95,
      direction = "y"
    ) +
    # right side y axis labels
    geom_text_repel(
      data = . %>% filter(!!Times == max(!!Times)),
      aes_(label = Grouping),
      hjust = "right",
      box.padding = 0.10,
      segment.color = "gray",
      segment.alpha = 0.6,
      fontface = "bold",
      size = YTextSize,
      nudge_x = 1.95,
      direction = "y"
    ) +
    # data point labels
    geom_label(aes_(label = Measurement),
      size = DataTextSize,
      # label.padding controls fill padding
      label.padding = unit(DataLabelPadding, "lines"),
      # label.size controls width of line around label box
      # 0 = no box line
      label.size = DataLabelLineSize,
      # color = text color of label
      color = DataTextColor,
      # fill background color for data label
      fill = DataLabelFillColor
    ) +
    MySpecial +
    labs(
      title = Title,
      subtitle = SubTitle,
      caption = Caption
    )

  # implicitly return plot object
} # end of function
