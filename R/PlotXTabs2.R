#' Bivariate bar (column) charts with statistical tests
#' 
#' Bivariate bar charts for nominal and ordinal data with 
#'   (optionally) statistical details included in the plot as a subtitle.
#'   
#' @param data A dataframe or tibble containing the `x` and `y` variables.
#' @param x The variable to plot on the X axis of the chart.
#' @param y The variable to segment the **columns** and test for independence.
#' @param counts If the dataframe is based upon counts rather than individual
#'   rows for observations, `counts` must contain the name of variable
#'   that contains the counts.  See `HairEyeColor` example.
#' @param results.subtitle Decides whether the results of statistical tests 
#'   are displayed as a subtitle (Default: TRUE). If set to FALSE, no subtitle.
#' @param paired Not used yet.
#' @param sampling.plan the sampling plan (see details in ?contingencyTableBF).
#' @param fixed.margin (see details in ?contingencyTableBF).
#' @param prior.concentration (see details in ?contingencyTableBF).
#' @param conf.level Scalar between 0 and 1. If unspecified, the defaults return
#'   lower and upper confidence intervals (0.95).
#' @param title The text for the plot title.
#' @param subtitle The text for the plot subtitle. **N.B** if statistical
#'   results are requested through `results.subtitle = TRUE` the results
#'   will have precedence.
#' @param caption The text for the plot caption. Please note the interaction
#'   with `bf.details`.
#' @param plottype one of four options "side", "stack", "mosaic" or "percent"
#' @param sample.size.label Logical that decides whether sample size information
#'   should be displayed for each level of the grouping variable `y`
#'   (Default: `TRUE`).
#' @param palette If a character string (e.g., `"Set1"`), will use that named
#'   palette. If a number, will index into the list of palettes of appropriate
#'   type. Default palette is `"Dark2"`.
#' @param perc.k Numeric that decides number of decimal places for percentage
#'   labels (Default: `0`).
#' @param labels.legend A character vector with custom labels for levels of
#'   the `y` variable displayed in the legend.
#' @param xlab Custom text for the `x` axis label (Default: `NULL`, which
#'   will cause the `x` axis label to be the `x` variable).
#' @param ylab Custom text for the `y` axis label (Default: `"Percent"`). Set
#'   to `NULL` for no label.
#' @param data.label Character decides what information needs to be displayed
#'   on the label in each bar segment. Possible options are `"percentage"`
#'   (default), `"counts"`, `"both"`.
#' @param legend.position The position of the legend
#'   `"none"`, `"left"`, `"right"`, `"bottom"`, `"top"` (Default: `"right"`).
#' @param x.axis.orientation The orientation of the `x` axis labels one of
#'   "slant" or "vertical" to change from the default horizontal
#'   orientation (Default: `NULL` which is horizontal).
#' @param label.text.size Numeric that decides size for bar labels
#'   (Default: `4`).
#' @param label.fill.color Character that specifies fill color for bar labels
#'   (Default: `white`).
#' @param label.fill.alpha Numeric that specifies fill color transparency or
#'   `"alpha"` for bar labels (Default: `1` range `0` to `1`).
#' @param bar.outline.color Character specifying color for bars (default: `"black"`).
#' @param package Name of package from which the palette is desired as string
#'   or symbol.
#' @param palette Name of palette as string or symbol.
#' @param ggtheme A function, ggplot2 theme name. Default value is ggplot2::theme_bw().
#'   Any of the ggplot2 themes, or themes from extension packages are allowed (e.g.,
#'   hrbrthemes::theme_ipsum(), etc.).
#' @param ggplot.component A ggplot component to be added to the plot prepared by
#'   ggstatsplot. Default is NULL. The argument should be entered as a function.
#'   If the given function has an argument axes.range.restrict and if it has been set
#'   to TRUE, the added ggplot component might not work as expected.
#' @param legend.title Title text for the legend.
#' @param k Number of digits after decimal point (should be an integer)
#'   (Default: k = 2) for statistical results.
#' @param direction Either `1` or `-1`. If `-1` the palette will be reversed.
#' @param bf.details Logical that decides whether to display additional 
#'   information from the Bayes Factor test in the caption (default:`FALSE`). 
#'   This will take precedence over any text you enter as a `caption`.
#' @param bf.display Character that determines how the Bayes factor value is
#'   is displayed.  The default is simply the number rounded to `k`. Other 
#'   options include "sensible", "log" and "support".
#' @param mosaic.offset Numeric that decides size of spacing between mosaic 
#'   blocks (Default: `.003` which is very narrow).  "reasonable" values
#'   probably lie between .05 and .001
#' @param mosaic.alpha Numeric that controls the "alpha" level of the mosaic 
#'   plot blocks (Default: `1` which is essentially no "fading"). Values must
#'   be in the range 0 to 1 see: `ggplot2::aes_colour_fill_alpha`
#'   
#'
#' @import ggplot2
#' @import ggmosaic
#'
#' @importFrom dplyr select group_by summarize n arrange if_else desc
#' @importFrom dplyr mutate mutate_at mutate_if filter_all
#' @importFrom rlang !! enquo quo_name
#' @importFrom paletteer scale_fill_paletteer_d
#' @importFrom tidyr uncount drop_na
#' @importFrom dplyr as_tibble
#' @importFrom scales label_percent
#' @importFrom BayesFactor extractBF
#' @importFrom BayesFactor contingencyTableBF
#' @importFrom sjstats crosstable_statistics
#' @importFrom scales label_percent
#'
#' @author Chuck Powell, Indrajeet Patil
#'
#'
#' @examples
#'
#' # for reproducibility
#' set.seed(123)
#'
#' # simplest possible call with the defaults
#' PlotXTabs2(
#'   data = mtcars,
#'   y = vs,
#'   x =  cyl
#' )  
#'
#' # more complex call
#' PlotXTabs2(
#'   data = datasets::mtcars,
#'   y = vs,
#'   x = cyl,
#'   bf.details = TRUE,
#'   labels.legend = c("0 = V-shaped", "1 = straight"),
#'   legend.title = "Engine Style",
#'   legend.position = "right",
#'   title = "The perenial mtcars example",
#'   palette = "Pastel1"
#' )
#'
#' PlotXTabs2(
#'   data = as.data.frame(HairEyeColor),
#'   y = Eye,
#'   x = Hair,
#'   counts = Freq
#' )
#' 
#'\dontrun{
#' # mosaic plot requires ggmosaic 0.2.2 or higher from github
#' PlotXTabs2(
#'   data = mtcars,
#'   x = vs,
#'   y =  am, 
#'   plottype = "mosaic", 
#'   data.label = "both", 
#'   mosaic.alpha = .9, 
#'   bf.display = "support", 
#'   title = "Motorcars Mosaic Plot VS by AM"
#' )
#'}
#'
#' @export

PlotXTabs2 <- function(data,
                    x,
                    y,
                    counts = NULL,
                    results.subtitle = TRUE,
                    title = NULL,
                    subtitle = NULL,
                    caption = NULL,
                    plottype = "percent",
                    xlab = NULL,
                    ylab = "Percent",
                    legend.title = NULL,
                    legend.position = "right",
                    labels.legend = NULL,
                    sample.size.label = TRUE,
                    data.label = "percentage",
                    label.text.size = 4,
                    label.fill.color = "white",
                    label.fill.alpha = 1,
                    bar.outline.color = "black",
                    x.axis.orientation = NULL,
                    conf.level = 0.95,
                    k = 2,
                    perc.k = 0,
                    mosaic.offset = .003,
                    mosaic.alpha = 1,
                    bf.details = FALSE,
                    bf.display = "regular",
                    sampling.plan = "jointMulti",
                    fixed.margin = "rows",
                    prior.concentration = 1,
                    paired = FALSE,
                    ggtheme = ggplot2::theme_bw(),
                    package = "RColorBrewer",
                    palette = "Dark2",
                    direction = 1,
                    ggplot.component = NULL) {
  

  # set default theme 
  ggplot2::theme_set(ggtheme)
  
  ### -----  input checking -----------

  if (base::missing(data)) {
    stop("You must specify a data source")
  }
  if (base::missing(x)) {
    stop("You must specify a x variable")
  }
  if (base::missing(y)) {
    stop("You must specify a y variable")
  }
  
  ### -----  x label and legend title  ==============
  # if legend title is not provided, use the variable name for 'y'
  if (is.null(legend.title)) {
    legend.title <- rlang::as_name(rlang::enquo(y))
  }

  # if not specified, use the variable name for 'x'
  if (is.null(xlab)) {
    xlab <- rlang::as_name(rlang::enquo(x))
  }

  ### -----  create temp local dataframe ====================
  # creating a dataframe based on whether counts
  if (base::missing(counts)) {
    data <-
      dplyr::select(
        .data = data,
        y = {{ y }},
        x = {{ x }}
      )
  } else {
    data <-
      dplyr::select(
        .data = data,
        y = !!rlang::enquo(y),
        x = !!rlang::quo_name(rlang::enquo(x)),
        counts = !!rlang::quo_name(rlang::enquo(counts))
      )
    data <- 
      data %>%
      tidyr::uncount(
        data = .,
        weights = counts,
        .remove = TRUE,
        .id = "id"
      )
  }
  
  original_N <- nrow(data)

  ### -----  calculate counts and percents -------

  # y and x need to be a factor or ordered factor
  # also drop the unused levels of the factors and NAs
  data <- data %>%
    dplyr::mutate_if(.tbl = ., not_a_factor, as.factor) %>%
    dplyr::mutate_if(.tbl = ., is.factor, droplevels) %>%
    dplyr::filter_all(.tbl = ., all_vars(!is.na(.)))
  
  valid_N <- nrow(data)
  missing_N <- original_N - valid_N
  
  # converting to tibble
  data <- data %>%
    dplyr::as_tibble(x = .)

  # convert the data into percentages; group by conditional variable
  df <-
    data %>%
    dplyr::group_by(.data = ., x, y) %>%
    dplyr::summarize(.data = ., counts = n()) %>%
    dplyr::mutate(.data = ., perc = (counts / sum(counts)) * 100) %>%
    dplyr::ungroup(x = .) %>%
    dplyr::arrange(.data = ., dplyr::desc(x = y)) %>%
    dplyr::filter(.data = ., counts != 0L)

  ### -----  bar segment labels ------------
  # checking what needs to be displayed on bar segments as labels and
  # the text size for the label; if both counts and percentages are going to
  # be displayed, then use a bit smaller text size
  if (data.label == "percentage") {
    # only percentage
    df <- df %>%
      dplyr::mutate(
        .data = .,
        data.label = paste0(round(x = perc, digits = perc.k), "%")
      )
  } else if (data.label == "counts") {
    # only raw counts
    df <- df %>%
      dplyr::mutate(
        .data = .,
        data.label = paste0("", counts)
      )
  } else if (data.label == "both") {
    # both raw counts and percentages
    df <- df %>%
      dplyr::mutate(
        .data = .,
        data.label = paste0(
          "",
          counts,
          "\n(",
          round(x = perc, digits = perc.k),
          "%)"
        )
      )
  }

  ### -----  sample size label =====================

  # if labels are to be displayed on the bottom of the charts
  # for each bar/column
  if (isTRUE(sample.size.label)) {
    df_n_label <- data %>%
      dplyr::group_by(.data = ., x) %>%
      dplyr::summarize(.data = ., N = n()) %>%
      dplyr::mutate(.data = ., N = paste0("(n = ", N, ")", sep = ""))
  }

  ### -----  preparing names for legend  ======================

  # getting labels for all levels of the 'y' variable factor
  if (is.null(labels.legend)) {
    legend.labels <- levels(df$y)
  } else if (!missing(labels.legend)) {
    legend.labels <- labels.legend
  }
  
  ### -----  start main plot ============================
  
  # plot
  if (plottype == "side") {
    p <- ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(fill = y, 
                             y = counts, 
                             x = x)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = position_dodge2(reverse = TRUE),
        color = bar.outline.color,
        na.rm = TRUE
      ) +
      ggplot2::geom_label(
        mapping = ggplot2::aes(label = data.label, 
                               group = y),
        show.legend = FALSE,
        position = position_dodge2(width = .9, reverse = TRUE),
        size = label.text.size,
        fill = label.fill.color,
        alpha = label.fill.alpha,
        na.rm = TRUE
      )
  } else if (plottype == "stack")  {
    p <- ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(fill = y, 
                             y = counts, 
                             x = x)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        color = bar.outline.color,
        na.rm = TRUE, 
        position = position_stack(reverse = TRUE)
      ) +
      ggplot2::geom_label(
        mapping = ggplot2::aes(label = data.label, 
                               group = y),
        show.legend = FALSE,
        position = position_stack(vjust = 0.5,
                                  reverse = TRUE),
        size = label.text.size,
        fill = label.fill.color,
        alpha = label.fill.alpha,
        na.rm = TRUE
      )
  }  else if (plottype == "mosaic")  {

    p <- 
      ggplot2::ggplot(data = df) +
      ggmosaic::geom_mosaic(aes(weight = counts, 
                                x = ggmosaic::product(x), 
                                fill = y),
                            offset = mosaic.offset,
                            alpha = mosaic.alpha) +
      scale_y_continuous(labels = scales::label_percent(accuracy = 1.0),
                         breaks = seq(from = 0, 
                                      to = 1, 
                                      by = 0.10),
                         minor_breaks = seq(from = 0.05, 
                                            to = 0.95, 
                                            by = 0.10))
    
    ### ---- Extract mosaic info and calculate cell pcts      
    mosaicgeominfo <- 
      ggplot2::ggplot_build(p)$data[[1]] %>% 
      group_by_at(vars(ends_with("__x"))) %>% 
      mutate(NN = sum(.wt)) %>% 
      mutate(pct = (.wt/NN))
    
    if (data.label == "percentage") {
      # only percentage
      mosaicgeominfo <- mosaicgeominfo %>%
        dplyr::mutate(
          .data = .,
          data.label = paste0(round(x = pct*100, 
                                    digits = perc.k), 
                              "%")
        )
    } else if (data.label == "counts") {
      # only raw counts
      mosaicgeominfo <- mosaicgeominfo %>%
        dplyr::mutate(
          .data = .,
          data.label = paste0("", .wt)
        )
    } else if (data.label == "both") {
      # both raw counts and percentages
      mosaicgeominfo <- mosaicgeominfo %>%
        dplyr::mutate(
          .data = .,
          data.label = paste0(
            "",
            .wt,
            "\n(",
            round(x = pct*100, digits = perc.k),
            "%)"
          )
        )
    }
    
    p <- p + geom_label(data = mosaicgeominfo, 
                        aes(x = (xmin + xmax)/2, 
                            y = (ymin + ymax)/2, 
                            label = data.label
                            ),
                        size = label.text.size,
                        fill = label.fill.color,
                        alpha = label.fill.alpha
                        )
    
  } else {
    p <- ggplot2::ggplot(
      data = df,
      mapping = ggplot2::aes(fill = y, 
                             y = perc, 
                             x = x)
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = ggplot2::position_fill(reverse = TRUE),
        color = bar.outline.color,
        na.rm = TRUE
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_percent(accuracy = 1.0),
        breaks = seq(from = 0, 
                     to = 1, 
                     by = 0.10),
        minor_breaks = seq(from = 0.05, 
                           to = 0.95, 
                           by = 0.10)
      ) +
      ggplot2::geom_label(
        mapping = ggplot2::aes(label = data.label, 
                               group = y),
        show.legend = FALSE,
        position = ggplot2::position_fill(reverse = TRUE, 
                                          vjust = 0.5),
        size = label.text.size,
        fill = label.fill.color,
        alpha = label.fill.alpha,
        na.rm = TRUE
      )
  }
  
  p <- p +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = legend.position
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = legend.title, 
                                                 reverse = TRUE)
    ) +
    paletteer::scale_fill_paletteer_d(
      palette = paste0(package, "::", palette),
      direction = direction,
      name = "",
      labels = unique(legend.labels)
    )
  
  ### -----  chi-square test (with Bayes) for caption and subtitle =============
  
  if (isTRUE(results.subtitle)) {
    chi.results <- sjstats::crosstable_statistics(data = data,
                                                  x1 = y,
                                                  x2 = x)
    chivalue <- round(chi.results$statistic, k)
    chidf <- chi.results$df
    ppvalue <- pvalr(chi.results$p.value)
    effecttype <- chi.results$method
    effectvalue <- round(chi.results$estimate, k)

    bf10_results <- 
      BayesFactor::extractBF(
        BayesFactor::contingencyTableBF(
          x = table(data$y, data$x),
          sampleType = sampling.plan,
          fixedMargin = fixed.margin,
          priorConcentration = prior.concentration
        ), onlybf = TRUE)
    
    bf_dtext <- bf_display(bf10_results, 
                           k = k,
                           display_type = bf.display)
    
    bf10_results <- round(bf10_results, k)
    bf01_results = round((1 / bf10_results), k)
    
    if (bf10_results >= 1) {
      subtitle <- bquote(~ chi['Ind']^2
                         * "[" * .(chidf)
                         * "]=" * .(chivalue)
                         * ", " * .(effecttype)
                         * "=" * bold(.(effectvalue))
                         * ", p=" * .(ppvalue)
                         * ", " * BF['10'] * .(bf_dtext)
                         * ", N=" *.(valid_N)
                         * ", missing=" *.(missing_N)
      )
      if (isTRUE(bf.details)) {
        caption <- bquote(~ BF['01']
                          * "=" * .(bf01_results)
                          * ", Sampling plan = " * .(sampling.plan)
                          * ", Prior concentration = " * .(prior.concentration)
        )
      }
    } else {
      subtitle <- bquote(~ chi['Ind']^2
                         * "[" * .(chidf)
                         * "]=" * .(chivalue)
                         * ", " * .(effecttype)
                         * "=" * bold(.(effectvalue))
                         * ", p=" * .(ppvalue)
                         * ", " * BF['01'] * .(bf_dtext)
                         * ", N=" *.(valid_N)
                         * ", missing=" *.(missing_N)
      )
      if (isTRUE(bf.details)) {
        caption <- bquote(~ BF['10']
                          * "=" * .(bf10_results)
                          * ", Sampling plan = " * .(sampling.plan)
                          * ", Prior concentration = " * .(prior.concentration)
        )
      }
    }
  } 
  
  ### -----  adding sample size info on x axis -------
  
  if (plottype == "percent" || plottype == "mosaic") {
    y_adjustment <- -0.05
  } else {
    y_adjustment <- -0.05 * max(df$counts)
    if (!is.null(ylab) && ylab == "Percent") {
      ylab <- "Counts"
    }
  }
  
  if (plottype != "mosaic") {
    if (isTRUE(sample.size.label)) {
      p <-
        p +
        ggplot2::geom_text(
          data = df_n_label,
          mapping = ggplot2::aes(
            x = x,
            y = y_adjustment,
            label = N,
            fill = NULL
          ),
          size = 4,
          na.rm = TRUE
        )
    }
  } else {
    if (isTRUE(sample.size.label)) {

      # Compute mosaic x axis label tick positions      
      xNlabelpos <- 
        mosaicgeominfo %>% 
        distinct(xNlabelpos = ((xmax - xmin)/2) + xmin) %>%
        pull(xNlabelpos)

      p <-
        p +
        ggplot2::geom_text(
          data = df_n_label,
          mapping = ggplot2::aes(
            x = xNlabelpos,
            y = y_adjustment,
            label = N,
            fill = NULL
          ),
          size = 4,
          na.rm = TRUE
        )
    }
  }
  
  ### -----  if we need to modify `x`-axis orientation ----
  if (!base::missing(x.axis.orientation)) {
    if (x.axis.orientation == "slant") {
      p <-
        p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 45,
            vjust = 1,
            hjust = 1,
            face = "bold"
          )
        )
    } else if (x.axis.orientation == "vertical") {
      p <-
        p + ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 90,
            vjust = 0.5,
            hjust = 1,
            face = "bold"
          )
        )
    }
  } else {
    p <-
      p + ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          face = "bold"
        )
      )
  }

  ### -----  add titles, captions and labels to plot ------
  p <-
    p +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      subtitle = subtitle,
      title = title,
      caption = caption
    )

  ### -----  adding optional ggplot.component ----------
  p <- p + ggplot.component

  ### -----  return the final plot -------
  return(p)
}
