# stable
# modified from https://www.rdocumentation.org/packages/BSDA/versions/1.2.0/topics/CIsim
#' Plotting random samples of confidence intervals around the mean
#'
#' This function takes some parameters and simulates random samples and
#' their confidence intervals
#'
#' @param samples The number of times to draw random samples
#' @param n The sample size we draw each time
#' @param mu The population mean mu
#' @param sigma The population standard deviation
#' @param conf.level What confidence level to compute 1 - alpha (significance level)
#'
#' @return A ggplot2 object
#' @export
#' @importFrom stats qnorm rnorm
#' @seealso \code{stats::qnorm}, \code{stats::rnorm}, \code{BSDA::CIsim}
#'
#' @author Chuck Powell
#'
#' @examples
#' OurConf(samples = 100, n = 30, mu = 0, sigma = 1, conf.level = 0.95)
#' OurConf(samples = 2, n = 5)
#' OurConf(samples = 25, n = 25, mu = 100, sigma = 20, conf.level = 0.99)
OurConf <- function(samples = 100, n = 30, mu = 0, sigma = 1, conf.level = 0.95) {
  alpha <- 1 - conf.level
  CL <- conf.level * 100
  n <- round(n)
  N <- round(samples)
  if (N <= 0 || n <= 1) {
    stop("Number of random samples and sample size must both be at least 2")
  }
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) ||
    conf.level <= 0 || conf.level >= 1)) {
    stop("'conf.level' must be a single number between 0 and 1")
  }
  if (sigma <= 0) {
    stop("Variance must be a positive value")
  }

  junk <- rnorm(N * n, mu, sigma)
  jmat <- matrix(junk, N, n)
  xbar <- apply(jmat, 1, mean)
  ll <- xbar - qnorm(1 - alpha / 2) * sigma / sqrt(n)
  ul <- xbar + qnorm(1 - alpha / 2) * sigma / sqrt(n)
  notin <- sum((ll > mu) + (ul < mu))
  percentage <- round((1 - notin / N) * 100, 2)
  data <- data.frame(xbar = xbar, ll = ll, ul = ul)
  data$samplenumb <- factor(as.integer(rownames(data)))
  data$correct <- "Includes"
  data$correct[data$ul < mu] <- "Low"
  data$correct[data$ll > mu] <- "High"
  bestfit <- function(NN = N) {
    list(
      scale_y_continuous(limits = c((mu - 2 * sigma), (mu + 2 * sigma))),
      if (NN >= 51) {
        scale_x_discrete(breaks = seq(0, 500, 10))
      }
    )
  }

  p <- ggplot(data, aes(y = xbar, x = samplenumb)) +
    geom_point() +
    geom_hline(yintercept = mu) +
    geom_errorbar(aes(ymin = ll, ymax = ul, color = correct), width = 0.3) +
    labs(
      title = bquote(.(N) ~ "random samples with" ~ .(CL) * "% confidence intervals where" ~ mu ~ "=" ~ .(mu) ~ "and" ~ sigma ~ "=" ~ .(sigma)),
      subtitle = bquote("Note:" ~ .(percentage) * "% of the confidence intervals contain" ~ mu ~ "=" ~ .(mu)),
      y = expression("Sample mean" ~ (bar(X))),
      x = paste0("Random samples of size = ", n),
      caption = ("modified from the CIsim function in package BSDA")
    ) +
    bestfit() +
    guides(color = guide_legend(title = NULL)) +
    theme_bw()
  print(p)
  cat(percentage, "% of the confidence intervals contain Mu =", mu, ".", "\n")
}
