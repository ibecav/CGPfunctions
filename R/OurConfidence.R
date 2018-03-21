# modified from https://www.rdocumentation.org/packages/BSDA/versions/1.2.0/topics/CIsim
OurConf(samples = 100, n = 30, mu = 0, sigma = 1, conf.level = 0.95)
OurConf(samples = 2, n =5)
OurConf()
OurConf(sigma = 0)

OurConf <- function (samples = 100, n = 30, mu = 0, sigma = 1, conf.level = 0.95) 
{
  if (!require(ggplot2)) {
    stop("Can't continue can't load ggplot2")
  }
  alpha <- 1 - conf.level
  CL <- conf.level * 100
  # round n and N in case user entered a non integer
  n <- round(n)
  N <- round(samples)
  # check for nonsense input
  if (N <= 0 || n <= 1) 
    stop("Number of random samples and sample size must both be at least 2")
  if (!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || 
                               conf.level <= 0 || conf.level >= 1)) 
    stop("'conf.level' must be a single number between 0 and 1")
  if (sigma <= 0) 
    stop("Variance must be a positive value")
  
  junk <- rnorm(N * n, mu, sigma)
  jmat <- matrix(junk, N, n)
  xbar <- apply(jmat, 1, mean)
  ll <- xbar - qnorm(1 - alpha/2) * sigma/sqrt(n)
  ul <- xbar + qnorm(1 - alpha/2) * sigma/sqrt(n)
  notin <- sum((ll > mu) + (ul < mu))
  percentage <- round((1 - notin/N) * 100, 2)
  data <- data.frame(xbar=xbar, ll=ll, ul=ul)
  data$samplenumb <- factor(as.integer(rownames(data)))
  data$correct <- "Includes"
  data$correct[data$ul < mu] <- "Low"
  data$correct[data$ll > mu] <- "High"
  bestfit <- function(NN = N) {
    list(
      scale_y_continuous(limits = c((mu - 2*sigma), (mu + 2*sigma))),
      if (NN>=51) 
        scale_x_discrete(breaks=seq(0,500,10))
    )
  }
  
  p<-ggplot(data,aes(y=xbar,x=samplenumb)) + 
    geom_point() + 
    geom_hline(yintercept = mu) +
    geom_errorbar(aes(ymin = ll, ymax = ul,color=correct), width = 0.3) +
    #    scale_y_continuous(limits = c((mu - 2*sigma), (mu + 2*sigma))) +
    labs(title = bquote(.(N)~"random samples with"~.(CL)*"% confidence intervals where"~mu~"="~.(mu)~"and"~sigma~"="~.(sigma)),
         subtitle = bquote("Note:"~.(percentage)*"% of the confidence intervals contain"~mu~"="~.(mu)), 
         y = expression("Sample mean"~(bar(X))), 
         x = paste0("Random samples of size = ", n),
         caption = ("modified from https://www.rdocumentation.org/packages/BSDA/versions/1.2.0/topics/CIsim")) +
    bestfit() +
    #        scale_x_discrete(breaks=seq(0,500,10)) +
    guides(color=guide_legend(title=NULL)) +
    theme_bw()
  print(p)
  cat(percentage, "% of the confidence intervals contain Mu =", mu, ".", "\n")
}
