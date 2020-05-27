#' SeeDist -- See The Distribution
#'
#' This function takes a vector of numeric data and returns one or more ggplot2
#' plots that help you visualize the data.  Meant to be a useful wrapper for
#' exploring univariate data.  Has a plethora of options including type of
#' visualization (histogram, boxplot, density, violin) as well as commonly
#' desired overplots like mean and median points, z and t curves etc..  Common
#' descriptive statistics are provided as a subtitle if desired and sent to the
#' console as well. 
#'
#' @param x the data to be visualized. Must be numeric.
#' @param title Optionally replace the default title displayed. title = NULL 
#'   will remove it entirely. title = "" will provide an empty title but 
#'   retain the spacing. A sensible default is provided otherwise.
#' @param subtitle Optionally replace the default subtitle displayed. subtitle = NULL 
#'   will remove it entirely. subtitle = "" will provide an empty subtitle but 
#'   retain the spacing. A sensible default is provided otherwise.
#' @param whatplots what type of plots?  The default is whatplots = c("d", "b", 
#'   "h", "v") for a density, a boxplot, a histogram, and a violin plot
#' @param numbins the number of bins to use for any plots that bin. If nothing is
#'   specified the function will calculate a rational number using Freedman-Diaconis
#'   via the \code{nclass.FD} function
#' @param var_explain additional contextual information about the variable as a string
#'   such as "Miles Per Gallon" which is appended to the default title information.
#' @param data.fill.color Character string that specifies fill color for the main data
#'   area (Default: `deepskyblue`).
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
#' @param xlab Custom text for the `x` axis label (Default: `NULL`, which
#'   will cause the `x` axis label to be the `x` variable).
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: k = 2) for statistical results.
#' @param add_jitter Logical (Default: `TRUE`) controls whether jittered data
#'   ponts are added to violin plot.
#' @param add_rug Logical (Default: `TRUE`) controls whether "rug" data
#'   points are added to density plot and histogram.
#' @param xlim_left,xlim_right Logical. For density plots can be used to 
#'   override the default which is 3 std deviations left and right of
#'   the mean of x. Useful for theoretical reasons like horsepower < 0
#'   or when `ggplot2` warns you that it has removed rows containing
#'   non-finite values (stat_density).
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
                    title = "Default",
                    subtitle = "Default",
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
                    add_jitter = TRUE,
                    add_rug = TRUE,
                    xlim_left = NULL,
                    xlim_right = NULL,
                    ggtheme = ggplot2::theme_bw()
                    ) {

  #### Basic setup ####
  
  # set default theme 
  ggplot2::theme_set(ggtheme)
  
  if (!is.numeric(x)) {
    stop("Sorry the data must be numeric")
  }
  
  x_name <- deparse(substitute(x)) # get the variable name
  
  # if not specified, use the variable name for x axis
  if (is.null(xlab)) {
    xlab <- x_name
  }
  
  # figure you what binwidth we'll use
  binnumber <- nclass.FD(x) # default
  
  binnumber <- ifelse(numbins == 0, 
                      binnumber, 
                      numbins)
  
  #### Get descriptives ####
  
  desc.output <- DescTools::Desc(x, 
                                 plotit = FALSE, 
                                 main = xlab, 
                                 digits = k)

  if (sum(is.na(x)) != 0) {
    missing_count <- sum(is.na(x))
    warning(paste("Removing",
                  missing_count,
                  "missing values"), 
            call. = FALSE)
    x <- x[!is.na(x)]
  }
  
  x_mean <- desc.output[[1]]$mean
  x_sd <- desc.output[[1]]$sd
  x_median <- desc.output[[1]]$quant['median']
  x_mode <- CGPfunctions::Mode(x)
  x_skew <- desc.output[[1]]$skew
  x_kurtosis <- desc.output[[1]]$kurt
    
  if (length(x_mode) >= 4) {
    warning(paste("There are", 
                  length(x_mode)), 
                  " modal values displaying just the first 3", 
            call. = FALSE)
    x_mode <- x_mode[c(1, 2, 3)]
  }
  
  #### Custom geoms ####
  
  my_jitter_geom <- list()
  if (add_jitter) {
    my_jitter_geom <- list(
      geom_jitter(aes(x = "", 
                      y = x),
                  width = 0.05, 
                  height = 0,
                  alpha = .5)
    )
  }
  
  my_rug_geom <- list()
  if (add_rug) {
    my_rug_geom <- list(
      geom_rug(aes(y = 0),
               sides = "b")
    )
  }
  
  if (is.null(xlim_left)) {
    xlim_left <- -3 * x_sd + x_mean
  }
  if (is.null(xlim_right)) {
    xlim_right <- +3 * x_sd + x_mean
  }
  
  #### Title, subtitle and caption ####
  
  if (!is.null(title) && title == "Default") {  
    my_title <- paste0("Distribution of the variable ", 
                       x_name, 
                       " ", 
                       var_explain
                      )
  } else {
    my_title <- title
  }
  
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
  
  if (!is.null(subtitle) && subtitle == "Default") {  
    my_subtitle <- make_subtitle(x,
                                 x_mean,
                                 x_sd,
                                 x_median,
                                 x_skew,
                                 x_kurtosis,
                                 k)
  } else {
    my_subtitle <- subtitle
  }
  
  mycaption <- bquote(bar(X) ~ "is" ~ .(mean.line.color) ~ 
                      ", Median is" ~ .(median.line.color) ~
                      ", Mode is" ~ .(mode.line.color) ~
                      ", z curve is" ~ .(zcurve.color) ~
                      ", t curve is" ~ .(tcurve.color)
                      )
  
  #### custom function to plot t curve ####
  
  custom_t_function <- function(x, mu, nu, df, ncp) {
    dt((x - mu)/nu, df, ncp) / nu
  }
  
  #### build the density plot ####
  
  if ("d" %in% tolower(whatplots)) {
    p <- ggplot(data.frame(x)) +
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
      my_rug_geom +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        x = xlab,
        caption = mycaption
      ) +
      xlim(xlim_left, 
           xlim_right) +
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

  
  #### build the boxplot ####
  
  if ("b" %in% tolower(whatplots)) {
    pp <- ggplot(data.frame(x)) +
      aes(x) +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        y = xlab,
        caption = mycaption
      ) +
      stat_boxplot(aes(x = "", 
                       y = x),
                   geom = "errorbar", 
                   width = 0.2) +
      geom_boxplot(aes(x = "", 
                       y = x), 
                   fill = data.fill.color, 
                   outlier.color = data.fill.color) +
      coord_flip() +
      geom_point(aes(x = "", 
                     y = x_mean), 
                 shape = mean.point.shape, 
                 size = mean.point.size, 
                 fill = mean.line.color) +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.major.y = element_blank()
      )
    print(pp)
  }
  
  #### build the histogram plot ####
  
  if ("h" %in% tolower(whatplots)) {
    ppp <- ggplot(data.frame(x)) +
      aes(x) +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        x = xlab,
        caption = mycaption
      ) +
      geom_histogram(bins = binnumber, 
                     color = "black", 
                     fill = data.fill.color) +
      my_rug_geom +
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
  
  #### build the violin plot ####
    
  if ("v" %in% tolower(whatplots)) {
    pppp <- ggplot(data.frame(x)) +
      aes(x) +
      labs(
        title = my_title,
        subtitle = my_subtitle,
        y = xlab,
        caption = mycaption
      ) +
      geom_violin(aes(x = "", 
                       y = x), 
                   fill = data.fill.color) +
      my_jitter_geom +
      coord_flip() +
      geom_point(aes(x = "", 
                     y = x_mean), 
                 shape = mean.point.shape, 
                 size = mean.point.size, 
                 fill = mean.line.color) +
      geom_point(aes(x = "", 
                     y = x_median), 
                 shape = median.point.shape, 
                 size = median.point.size, 
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
  
  #### return output to console ####
  return(desc.output)
} # end function
