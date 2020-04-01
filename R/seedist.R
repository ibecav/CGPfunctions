#' See The Distribution
#'
#' This function takes a vector of numeric data and returns one or more ggplot2
#' plots that help you visualize the data.
#'
#' @param x the data to be visualized must be numeric.
#' @param numbins the number of bins to use for any plots that bin. If nothing is
#'   specified the function will calculate a rational number using Freedman-Diaconis
#'   via the \code{nclass.FD} function
#' @param var_explain additional contextual information about the variable as a string
#'   such as "Miles Per Gallon"
#' @param data.fill.color Character string that specifies fill color for our data
#'   (Default: `deepskyblue`).
#' @param mean.line.color,median.line.color,mode.line.color Character string that 
#'   specifies line color (Default: `darkgreen`, `yellow`, `orange`).
#' @param mean.line.type,median.line.type,mode.line.type Character string that 
#'   specifies line color (Default: `longdash`, `dashed`, `dashed`).
#' @param mean.line.size,median.line.size,mode.line.size Numeric that 
#'   specifies line size (Default: `1.5`, `1.5`, `1`).  You can set to `0` to make
#'   any of the lines "disappear".
#' @param mean.point.shape,median.point.shape Integer in 0 - 25 
#'   specifies shape of mean or median point mark on the violin plot
#'   (Default: `21`, `23`).
#' @param mean.point.size,median.point.size Integer  
#'   specifies size of mean or median point mark on the violin plot
#'   (Default: `4`). You can set to `0` to make any of the points "disappear".
#' @param zcurve.color,tcurve.color Character string that 
#'   specifies line color (Default: `red`, `black`).
#' @param zcurve.type,tcurve.type Character string that 
#'   specifies line color (Default: `twodash`, `dotted`).
#' @param zcurve.size,tcurve.size Numeric that 
#'   specifies line size (Default: `1`).  You can set to `0` to make
#'   any of the lines "disappear".
#' @param whatplots what type of plots?  The default is whatplots = c("d", "b", "h", "v")
#'   for a density, a boxplot, a histogram, and a violin plot
#' @param xlab Custom text for the `x` axis label (Default: `NULL`, which
#'   will cause the `x` axis label to be the `x` variable).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: k = 2) for statistical results.
#' @param ggtheme A function, ggplot2 theme name. Default value is ggplot2::theme_bw().
#'   Any of the ggplot2 themes, or themes from extension packages are allowed (e.g.,
#'   hrbrthemes::theme_ipsum(), etc.).
#'
#' @return from 1 to 4 plots depending on what the user specifies as well as an
#'   extensive summary courtesy `DescTools::Desc` printed to the console
#'
#' @export
#' @import ggplot2
#' @importFrom grDevices nclass.FD
#' @importFrom stats dnorm dt median sd
#' @importFrom DescTools Desc
#'
#' @section Warning:
#'   If the data has more than 3 modal values only the first three of them are plotted.
#'   The rest are ignored and the user is warned on the console.
#'
#' Missing values are removed with a warning to the user
#'
#' @seealso \code{\link[grDevices]{nclass}}
#'
#' @examples
#' SeeDist(rnorm(100, mean = 100, sd = 20), numbins = 15, var_explain = "A Random Sample")
#' SeeDist(mtcars$hp, var_explain = "Horsepower", whatplots = c("d", "b"))
#' SeeDist(iris$Sepal.Length, var_explain = "Sepal Length", whatplots = "d")
#' @author Chuck Powell
#'
SeeDist <- function(x, 
                    numbins = 0, 
                    xlab = NULL,
                    var_explain = NULL, 
                    data.fill.color = "deepskyblue",
                    mean.line.color = "darkgreen",
                    median.line.color = "yellow",
                    mode.line.color = "orange",
                    mean.line.type = "longdash",
                    median.line.type = "dashed",
                    mode.line.type = "dashed",
                    mean.line.size = 1.5,
                    median.line.size = 1.5,
                    mean.point.shape = 21,
                    median.point.shape = 23,
                    mean.point.size = 4,
                    median.point.size = 4,
                    zcurve.color = "red",
                    zcurve.type = "twodash",
                    zcurve.size = 1,
                    tcurve.color = "black",
                    tcurve.type = "dotted",
                    tcurve.size = 1,
                    mode.line.size = 1,
                    whatplots = c("d", "b", "h", "v"),
                    k = 2,
                    ggtheme = ggplot2::theme_bw()
                    ) {

  # set default theme 
  ggplot2::theme_set(ggtheme)
  
  if (!is.numeric(x)) {
    stop("Sorry the data must be numeric")
  }
  
  x_name <- deparse(substitute(x)) # get the variable name
  
  # if not specified, use the variable name for 'x'
  if (is.null(xlab)) {
    xlab <- x_name
  }
  
  my_title <- paste0("Distribution of the variable ", 
                     deparse(substitute(x)), 
                     " ", var_explain
                    )
  
  
  if (sum(is.na(x)) != 0) {
    missing_count <- sum(is.na(x))
    warning(paste("Removing",
                  missing_count,
                  "missing values"), 
            call. = FALSE)
    x <- x[!is.na(x)]
  }
  
  x_mean <- mean(x, na.rm = TRUE) # store the mean
  x_sd <- sd(x, na.rm = TRUE) # store the sd
  x_median <- median(x, na.rm = TRUE)
  x_mode <- CGPfunctions::Mode(x)
  
  if (length(x_mode) >= 4) {
    warning(paste("There are", 
                  length(x_mode)), 
                  " modal values displaying just the first 3", call. = FALSE)
    x_mode <- x_mode[c(1, 2, 3)]
  }
  
  x_skew <- sum((x - mean(x, na.rm = TRUE))^3) / 
                (length(x[!is.na(x)]) * sd(x, na.rm = TRUE)^3)
  
  x_kurtosis <- sum((x - mean(x, na.rm = TRUE))^4) / 
                    (length(x[!is.na(x)]) * sd(x, na.rm = TRUE)^4) - 3
  
  binnumber <- nclass.FD(x)
  
  binnumber <- ifelse(numbins == 0, 
                      binnumber, 
                      numbins)
  
  make_subtitle <- 
    function(x,
             mean_x,
             sd_x,
             median_x,
             Skew_x,
             Kurtosis_x,
             k = k) {
      ret_subtitle <- bquote("N =" ~ .(length(x)) * 
                              "," ~ bar(X) ~ "=" ~ .(round(mean_x, k)) * 
                              ", SD =" ~ .(round(sd_x, k)) * 
                              ", Median =" ~ .(round(median_x, k)) * 
                              ", Skewness =" ~ .(round(Skew_x, k)) * 
                              ", Kurtosis =" ~ .(round(Kurtosis_x, k)
                            )
      )
    }
  
  my_subtitle <- make_subtitle(x,
                               x_mean,
                               x_sd,
                               x_median,
                               x_skew,
                               x_kurtosis,
                               k)
  
  custom_t_function <- function(x, mu, nu, df, ncp) {
    dt((x - mu)/nu, df, ncp) / nu
  }
  
  # build the first plot
  if ("d" %in% tolower(whatplots)) {
    p <- ggplot() +
      aes(x) +
      geom_density(fill = data.fill.color, ) +
      stat_function(fun = dnorm, 
                    color = zcurve.color, 
                    linetype = zcurve.type,
                    size = zcurve.size,
                    args = list(mean = x_mean, 
                                sd = x_sd)
                    ) +
      stat_function(fun = custom_t_function, 
                    color = tcurve.color,
                    linetype = tcurve.type,
                    size = tcurve.size,
                    args = list(mu = x_mean, 
                              nu = x_sd, 
                              df = length(x) - 1, 
                              ncp = 0)
      ) +
      geom_vline(xintercept = x_mean, 
                 colour = mean.line.color, 
                 linetype = mean.line.type, 
                 size = mean.line.size) +
      geom_vline(xintercept = x_median, 
                 colour = median.line.color, 
                 linetype = median.line.type, 
                 size = median.line.size) +
      geom_vline(xintercept = x_mode, 
                 colour = mode.line.color, 
                 linetype = mode.line.type,
                 size = mode.line.size) +
      geom_rug(aes(y = 0)) +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        x = xlab,
        caption = (bquote(bar(X) ~ " = green, Median = yellow, Mode(s) = orange, Blue = density plot, Red = theoretical normal"))
      ) +
      xlim(-3 * sd(x) + mean(x), 
           +3 * sd(x) + mean(x)) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    print(p)
  }

  # build the second plot
  if ("b" %in% tolower(whatplots)) {
    pp <- ggplot() +
      aes(x) +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        y = xlab,
        caption = (bquote(bar(X) ~ " displayed as a red dot, Median as a black line, and outlier(s) as small dark red dots"))
      ) +
      stat_boxplot(aes(x = "", 
                       y = x),
                   geom = "errorbar", width = 0.2) +
      geom_boxplot(aes(x = "", 
                       y = x), 
                   fill = data.fill.color, 
                   outlier.color = "dark red") +
      coord_flip() +
      geom_point(aes(x = "", 
                     y = x_mean), 
                 shape = 21, 
                 size = 4, 
                 color = "white", 
                 fill = "red") +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank()
      )
    print(pp)
  }
  # build the third plot
  if ("h" %in% tolower(whatplots)) {
    ppp <- ggplot() +
      aes(x) +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        x = xlab,
        caption = (bquote(bar(X) ~ " displayed as a green line, Median as a yellow line, and Mode(s) as orange line(s)"))
      ) +
      geom_histogram(bins = binnumber, 
                     color = "black", 
                     fill = data.fill.color) +
      geom_rug(aes(y = 0)) +
      geom_vline(xintercept = x_mean, 
                 colour = mean.line.color, 
                 linetype = mean.line.type, 
                 size = mean.line.size) +
      geom_vline(xintercept = x_median, 
                 colour = median.line.color, 
                 linetype = median.line.type, 
                 size = median.line.size) +
      geom_vline(xintercept = x_mode, 
                 colour = mode.line.color, 
                 linetype = mode.line.type,
                 size = mode.line.size)
    print(ppp)
  }
  # build the second plot
  if ("v" %in% tolower(whatplots)) {
    pppp <- ggplot() +
      aes(x) +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        y = xlab,
        caption = (bquote(bar(X) ~ " displayed as a red dot, Median as a black diamond, and outlier(s) as small dark red dots"))
      ) +
      geom_violin(aes(x = "", 
                       y = x), 
                   fill = data.fill.color) +
      geom_jitter(aes(x = "", 
                      y = x),
                  width = 0.05, 
                  height = 0,
                  alpha = .5) +
      coord_flip() +
      geom_point(aes(x = "", 
                     y = x_mean), 
                 shape = mean.point.shape, 
                 size = mean.point.size, 
                 color = "white", 
                 fill = mean.line.color) +
      geom_point(aes(x = "", 
                     y = x_median), 
                 shape = median.point.shape, 
                 size = median.point.size, 
                 color = "white", 
                 fill = median.line.color) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank()
      )
    print(pppp)
  }
  text.output <- DescTools::Desc(x, plotit = FALSE, main = xlab, digits = k)
  return(text.output)
} # end function
