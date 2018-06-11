# https://github.com/leeper/slopegraph
# https://ibecav.github.io/slopegraph/
# https://rpubs.com/hadley/97970
# load(newcancer.rda)
# head(newcancer)

#' Plot a Slopegraph a la Tufte using dplyr and ggplot2
#'
#' Takes a dataframe as input, with three named columns being used to plot.
#' Makes the required adjustments to the ggplot2 parameters and returns the plot.
#'
#' @param dataframe a dataframe or an object that can be coerced to a dataframe. Basic error checking is performed.
#' @param Times a column inside the dataframe that will be plotted on the x axis. Traditionally this is some measure of time.  The function accepts a column of class ordered, factor or character.
#' @param Measurement a column inside the dataframe that will be plotted on the y axis. Traditionally this is some measure such as a percentage.  Currently the function accepts a column of type integer or numeric.
#' @param Grouping a column inside the dataframe that will be used to group and distinguish measurements.
#' @param Title Optionally the title to be displayed. Title = NULL will remove it entirely. Title = "" will provide and empty title but retain the sapcing.
#' @param SubTitle Optionally the sub-title to be displayed.  SubTitle = NULL will remove it entirely. SubTitle = "" will provide and empty title but retain the sapcing.
#' @param Caption Optionally the caption to be displayed. Caption = NULL will remove it entirely. Caption = "" will provide and empty title but retain the sapcing.
#'
#' @return a plot of type ggplot to the default plot device
#' @export
#' @import ggplot2
#' @importFrom dplyr filter mutate group_by summarise %>% n
#' @importFrom ggrepel geom_text_repel
#'
#' @author Chuck Powell
#' @seealso \code{\link{newcancer}}, \code{\link[car]{leveneTest}},
#' @examples
#'
#' newggslopegraph(newcancer,Year,Survival,Type)
#'
#'
newggslopegraph <- function(dataframe, Times, Measurement, Grouping,
                            Title = "No title given",
                            SubTitle = "No subtitle given",
                            Caption = "No caption given")
  {
  # error checking and setup
  theme_set(theme_bw())
  # Since ggplot2 objects are just regular R objects, put them in a list
  MySpecial <- list(
    # Format tweaks
    scale_x_discrete(position = "top"), # move the x axis labels up top
    theme(legend.position  = "none"), # Remove the legend
    theme(panel.border     = element_blank()), # Remove the panel border
    theme(axis.title.y     = element_blank()), # Remove just about everything from the y axis
    theme(axis.text.y      = element_blank()),
    theme(panel.grid.major.y = element_blank()),
    theme(panel.grid.minor.y = element_blank()),
    theme(axis.title.x     = element_blank()), # Remove a few things from the x axis
    theme(panel.grid.major.x = element_blank()),
    theme(axis.text.x.top      = element_text(size=12)), # and increase font size
    theme(axis.ticks       = element_blank()), # Remove x & y tick marks
    theme(plot.title       = element_text(size=14, face = "bold")), # Format title
    theme(plot.title       = element_text(hjust = 0.5)), # Center title & subtitle
    theme(plot.subtitle    = element_text(hjust = 0.5))
  )
  if (length(match.call()) <= 4) {
    stop("Not enough arguments passed... requires a dataframe, plus at least three variables")
  }
  argList <-  as.list(match.call()[-1])
  if (!exists(deparse(substitute(dataframe)))) {
    stop("The first object in your list does not exist. It should be a dataframe")
  }
  if (!is(dataframe, "data.frame")) {
    stop("The first name you passed does not appear to be a data frame")
  }
  if (!deparse(substitute(Times)) %in% names(dataframe)) {
    stop("Times failure")
  }
  if (!deparse(substitute(Measurement)) %in% names(dataframe)) {
    stop("Measurement failure")
  }
  if (!deparse(substitute(Grouping)) %in% names(dataframe)) {
    stop("Grouping failure")
  }
  if (!class(dataframe[[deparse(substitute(Measurement))]]) %in% c("integer","numeric")) {
    stop("Sorry I need the measured variable to be a number")
  }
  if (!"ordered" %in% class(dataframe[[deparse(substitute(Times))]])) { # keep checking
    if (!"character" %in% class(dataframe[[deparse(substitute(Times))]])) { # keep checking
      if ("factor" %in% class(dataframe[[deparse(substitute(Times))]])) { # impose order
        warning("Converting to an ordered factor", call. = FALSE)
        dataframe[[deparse(substitute(Times))]] <- factor(dataframe[[deparse(substitute(Times))]], ordered = TRUE)
      } else {
        stop("Sorry I need the Times variable to be of class character, factor or ordered")
      }
    }
  }

  Times <- enquo(Times)
  Measurement <- enquo(Measurement)
  Grouping <- enquo(Grouping)

    dataframe %>%
      filter(!is.na(!! Times), !is.na(!! Measurement), !is.na(!! Grouping))  %>%
#      mutate(!!quo_name(Times) := factor(!!Times), !!quo_name(Measurement) := factor(!!Measurement)) %>%
      ggplot(aes_(group=Grouping, y=Measurement, x=Times)) +
        geom_line(aes_(color = Grouping, alpha = 1), size = 1) +
        geom_text_repel(data = dataframe %>% filter(!! Times == min(!! Times)),
                        aes_(label = Grouping) ,
                        hjust = "left",
                        fontface = "bold",
                        size = 3,
                        nudge_x = -.45,
                        direction = "y") +
        geom_text_repel(data = dataframe %>% filter(!! Times == max(!! Times)),
                        aes_(label = Grouping),
                        hjust = "right",
                        fontface = "bold",
                        size = 3,
                        nudge_x = .5,
                        direction = "y") +
        geom_label(aes_(label = Measurement), size = 2.5, label.padding = unit(0.05, "lines"), label.size = 0.0) +
        MySpecial +
        labs(
              title = Title,
              subtitle = SubTitle,
              caption = Caption
            )
} # end of function

# newggslopegraph(newcancer,Year,Survival,Type)

# title = "Estimates of Percent Survival Rates"
# subtitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176."
# caption = "https://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=0003nk"

