#' See The Distribution
#'
#' This function takes a vector of numeric data and returns one or more ggplot2
#' plots that help you visualize the data
#'
#' @param qqq the data to be visualized
#' @param numbins the number of bins to use
#' @param whatvar additional information about the variable
#'
#' @return from 1 to 3 plots depending on what the user specifies
#' @export
#' @import ggplot2
#' @importFrom grDevices nclass.FD
#' @importFrom stats dnorm dt median
#'
#' @examples
#' SeeDist(rnorm(100, mean=100, sd=20))
#' SeeDist(mtcars$hp, whatvar = "Horsepower")
#' SeeDist(iris$Sepal.Length, whatvar = "Sepal Length")
#' 
SeeDist <- function (qqq, numbins = 0, whatvar = "Unspecified")
{
  # error checking
  if (!requireNamespace("ggplot2")) {
    stop("Can't continue can't load ggplot2")
  }
  theme_set(theme_bw())
  if (!is.numeric(qqq)) {
      stop("Sorry the data must be numeric")
  }
   xxx<- deparse(substitute(qqq)) # get the variable name
   meanqqq<-mean(qqq,na.rm = TRUE) # store the mean
   sdqqq<-sd(qqq,na.rm = TRUE) # store the sd
   medianqqq<-median(qqq,na.rm = TRUE)
   modeqqq<-Mode(qqq)
   if (length(modeqqq) >= 4) {
     modeqqq <- modeqqq[c(1,2,3)]
   }
   Skewqqq<-sum((qqq - mean(qqq,na.rm=TRUE))^3)/(length(qqq[!is.na(qqq)]) * sd(qqq,na.rm=TRUE)^3)
   Kurtosisqqq<-sum((qqq - mean(qqq, na.rm = TRUE))^4)/(length(qqq[!is.na(qqq)]) * sd(qqq, na.rm = TRUE)^4) - 3
   binnumber <- nclass.FD(qqq)
   binnumber <- ifelse(numbins == 0, binnumber, numbins)
   custom <- function(x) {dt((qqq - meanqqq), df =length(qqq))}
#   print(length(modeqqq))
   
# build the plot
  p<-ggplot() +
    aes(qqq) +
    stat_function(fun = dnorm, color="red", args=list(mean=meanqqq, sd=sdqqq)) +
    geom_density() +
    geom_vline(xintercept = meanqqq, colour="dark green", linetype="dashed", size=1.5) +
    geom_vline(xintercept = medianqqq, colour="yellow", linetype="dashed", size=1.5) +
    geom_vline(xintercept = modeqqq, colour="orange", linetype="dashed") +
    geom_rug(aes(y = 0)) +
    labs(title = paste0("Distribution of the variable ", xxx, " (", whatvar, ")"),
         subtitle = bquote("N ="~.(length(qqq))*","~bar(X)~"="~.(round(meanqqq,1))*", SD ="~.(round(sdqqq,2))*", Median ="~.(round(medianqqq,2))*", Skewness ="~.(round(Skewqqq,2))*", Kurtosis ="~.(round(Kurtosisqqq,2))),
         x = whatvar,
         caption = ("January 31, 2018")) +
    xlim(-3 * sd(qqq) + mean(qqq),3 * sd(qqq) + mean(qqq)) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank(),
          panel.grid.major.y=element_blank(),
          panel.grid.minor.y=element_blank())
  # build the second plot
  pp<-ggplot() +
    aes(qqq) +
    labs(title = paste0("Distribution of the variable ", xxx, " (", whatvar, ")"),
         subtitle = bquote("N ="~.(length(qqq))*","~bar(X)~"="~.(round(meanqqq,1))*", SD ="~.(round(sdqqq,2))*", Median ="~.(round(medianqqq,2))*", Skewness ="~.(round(Skewqqq,2))*", Kurtosis ="~.(round(Kurtosisqqq,2))),
         y = whatvar,
         caption = ("January 31, 2018")) +
    geom_boxplot(aes(x = "", y = qqq), fill = "blue", outlier.color = "dark red") +
  coord_flip() +
  geom_point(aes(x = "", y = meanqqq), shape=21, size=4, color="white", fill="red") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        panel.grid.major.y=element_blank())
  
# build the third plot
  ppp<-ggplot() +
    aes(qqq) +
    labs(title = paste0("Distribution of the variable ", xxx, " (", whatvar, ")"),
         subtitle = bquote("N ="~.(length(qqq))*","~bar(X)~"="~.(round(meanqqq,1))*", SD ="~.(round(sdqqq,2))*", Median ="~.(round(medianqqq,2))*", Skewness ="~.(round(Skewqqq,2))*", Kurtosis ="~.(round(Kurtosisqqq,2))),
         x = whatvar,
         caption = ("January 31, 2018")) +
    geom_histogram(bins=binnumber,color = "black",fill="blue") +
    geom_vline(xintercept = meanqqq, colour="dark green", linetype="dashed", size=1.5) +
    geom_vline(xintercept = medianqqq, colour="yellow", linetype="dashed", size=1.5) +
    geom_vline(xintercept = modeqqq, colour="orange", linetype="dashed") 

  rrr<-list(p,pp,ppp)
#  print(rrr)
  return(rrr)
} # end function

