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
#' @param LineThickness Optionally the thickness of the plotted lines. LineThickness = 1 
#' is the default must be a numeric.
#' @param DataTextSize Optionally the font size of the plotted data points. DataTextSize = 2.5 
#' is the default must be a numeric.
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
#' @param RemoveMissing logical, by default set to \code{TRUE} so that if any \code{Measurement}
#' is missing \bold{all rows} for that \code{Grouping} are removed. If set to \code{FALSE} then
#' the function will try to remove and graph what data it does have. \bold{N.B.} missing values
#' for \code{Times} and \code{Grouping} are never permitted and will generate a fatal error with
#' a warning. 
#' 
#' 
#' @return a plot of type ggplot to the default plot device
#' @export
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by summarise %>% n
#' @importFrom ggrepel geom_text_repel
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
#'                 Title = "Estimates of Percent Survival Rates",
#'                 SubTitle = NULL,
#'                 Caption = NULL)
#'                 
#' # simple formatting changes
#' newggslopegraph(newcancer, Year, Survival, Type, 
#'                 Title = "Estimates of Percent Survival Rates", 
#'                 LineColor = "darkgray", 
#'                 LineThickness = .5, 
#'                 SubTitle = NULL, 
#'                 Caption = NULL)
#'                 
#' # complex formatting with recycling and wider labels see vignette for more examples
#' newggslopegraph(newcancer, Year, Survival, Type, 
#'                 Title = "Estimates of Percent Survival Rates", 
#'                 SubTitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
#'                 Caption = "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk",
#'                 LineColor = c("black", "red", "grey"), 
#'                 LineThickness = .5,
#'                 WiderLabels = TRUE)
#'
#'
newggslopegraph <- function(dataframe, Times, Measurement, Grouping,
                            Title = "No title given",
                            SubTitle = "No subtitle given",
                            Caption = "No caption given",
                            XTextSize = 12,
                            YTextSize = 3,
                            TitleTextSize = 14,
                            SubTitleTextSize = 10,
                            CaptionTextSize = 8,
                            LineThickness = 1,
                            LineColor = "ByGroup",
                            DataTextSize = 2.5,
                            WiderLabels = FALSE,
                            RemoveMissing = TRUE)
  {
  . = NULL # appease CRAN since you can't import this convention from dplyr or ggplot2
  # Since ggplot2 objects are just regular R objects, put them in a list
  MySpecial <- list(
    # Format tweaks
    scale_x_discrete(position = "top"), # move the x axis labels up top
    theme_bw(),
    theme(legend.position  = "none"), # Remove the legend
    theme(panel.border     = element_blank()), # Remove the panel border
    theme(axis.title.y     = element_blank()), # Remove just about everything from the y axis
    theme(axis.text.y      = element_blank()),
    theme(panel.grid.major.y = element_blank()),
    theme(panel.grid.minor.y = element_blank()),
    theme(axis.title.x     = element_blank()), # Remove a few things from the x axis
    theme(panel.grid.major.x = element_blank()),
    theme(axis.text.x.top      = element_text(size = XTextSize)), # and increase font size
    theme(axis.ticks       = element_blank()), # Remove x & y tick marks
    theme(plot.title       = element_text(size = TitleTextSize, face = "bold")), # Format title
    theme(plot.title       = element_text(hjust = 0.5)), # Center title & subtitle
    theme(plot.subtitle    = element_text(hjust = 0.5, size = SubTitleTextSize)),
    theme(plot.caption     = element_text(size = CaptionTextSize))
  )
  # for convenience store these
  Ndataframe <- deparse(substitute(dataframe)) # name of dataframe
  NTimes <- deparse(substitute(Times)) # name of Times variable
  NMeasurement <- deparse(substitute(Measurement)) # name of Measurement variable
  NGrouping <- deparse(substitute(Grouping)) # name of Grouping variable
  # error checking and setup
  if (length(match.call()) <= 4) {
    stop("Not enough arguments passed... requires a dataframe, plus at least three variables")
  }
  argList <-  as.list(match.call()[-1])
  if (!exists(Ndataframe)) {
    stop("The first object in your list '", Ndataframe ,"' does not exist. It should be a dataframe", call. = FALSE)
  }
  if (!is(dataframe, "data.frame")) {
    stop(paste0("'", Ndataframe, "' does not appear to be a data frame"), call. = FALSE)
  }
  if (!NTimes %in% names(dataframe)) {
    stop(paste0("'", NTimes, "' is not the name of a variable in '", Ndataframe, "'"), call. = FALSE)
  }
  if (anyNA(dataframe[[NTimes]])) {
    stop(paste0("'", NTimes, "' can not have missing data please remove those rows!"), call. = FALSE)
  }
  if (!NMeasurement %in% names(dataframe)) {
    stop(paste0("'", NMeasurement, "' is not the name of a variable in '", Ndataframe, "'"), call. = FALSE)
  }
  if (!NGrouping %in% names(dataframe)) {
    stop(paste0("'", NGrouping, "' is not the name of a variable in '", Ndataframe, "'"), call. = FALSE)
  }
  if (anyNA(dataframe[[NGrouping]])) {
    stop(paste0("'", NGrouping, "' can not have missing data please remove those rows!"), call. = FALSE)
  }
  if (!class(dataframe[[NMeasurement]]) %in% c("integer","numeric")) {
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

  NumbOfLevels <- nlevels(factor(dataframe[[NTimes]]))
  if (WiderLabels) {
    MySpecial <- c(MySpecial, expand_limits(x = c(0, NumbOfLevels+1)))
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
                  group_by(!! Grouping) %>% 
                  filter(!anyNA(!! Measurement)) %>%
                  droplevels()
    } else {
      dataframe <- dataframe %>%
        filter(!is.na(!! Measurement))
    }
  }
  
    dataframe %>%
      ggplot(aes_(group=Grouping, y=Measurement, x=Times)) +
        LineGeom +
        geom_text_repel(data = . %>% filter(!! Times == min(!! Times)),
                        aes_(label = Grouping) ,
                        hjust = "left",
                        box.padding = 0.10,
                        segment.color = "gray",
                        segment.alpha = 0.6,
                        fontface = "bold",
                        size = YTextSize,
                        nudge_x = -1.95,
                        direction = "y") +
        geom_text_repel(data = . %>% filter(!! Times == max(!! Times)),
                        aes_(label = Grouping),
                        hjust = "right",
                        box.padding = 0.10,
                        segment.color = "gray",
                        segment.alpha = 0.6,
                        fontface = "bold",
                        size = YTextSize,
                        nudge_x = 1.95,
                        direction = "y") +
        geom_label(aes_(label = Measurement), 
                   size = DataTextSize, 
                   label.padding = unit(0.05, "lines"), 
                   label.size = 0.0) +
        MySpecial +
        labs(
              title = Title,
              subtitle = SubTitle,
              caption = Caption
            )
} # end of function

