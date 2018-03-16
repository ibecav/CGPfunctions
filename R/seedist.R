#' See The Distribution
#'
#' This function takes a vector of numeric data and returns one or more ggplot2
#' plots that help you visualize the data
#'
#' @param qqq the data to be visualized must be numeric.
#' @param numbins the number of bins to use for any plots that bin. If nothing is
#' specified the function will calculate a rational number using Freedman-Diaconis
#' via the \code{nclass.FD} function
#' @param whatvar additional contextual information about the variable as a string
#' such as "Miles Per Gallon"
#'
#' @return from 1 to 3 plots depending on what the user specifies
#' @export
#' @import ggplot2
#' @importFrom grDevices nclass.FD
#' @importFrom stats dnorm dt median
#'
#' @section Warning:
#' If the data has more than 3 modal values only the first three of them are plotted.
#' The rest are ignored and the user is warned on the console.
#'
#' \code{Mode(NA)} is \code{NA} and a vector where the majority of entries
#' are \code{NA} is also NA
#' 
#' @seealso \code{\link[grDevices]{nclass}}
#' 
#' @examples
#' SeeDist(rnorm(100, mean=100, sd=20), numbins = 15, whatvar = "A Random Sample")
#' SeeDist(mtcars$hp, whatvar = "Horsepower")
#' SeeDist(iris$Sepal.Length, whatvar = "Sepal Length")
#' 
#' @author Chuck Powell
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
     warning(paste("There are", length(modeqqq)), " modal values displaying just the first 3")
     modeqqq <- modeqqq[c(1,2,3)]
   }
   Skewqqq<-sum((qqq - mean(qqq,na.rm=TRUE))^3)/(length(qqq[!is.na(qqq)]) * sd(qqq,na.rm=TRUE)^3)
   Kurtosisqqq<-sum((qqq - mean(qqq, na.rm = TRUE))^4)/(length(qqq[!is.na(qqq)]) * sd(qqq, na.rm = TRUE)^4) - 3
   binnumber <- nclass.FD(qqq)
   binnumber <- ifelse(numbins == 0, binnumber, numbins)
   custom <- function(x) {dt((qqq - meanqqq), df =length(qqq))}

# build the first plot
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
         caption = (bquote(bar(X)~" = green, Median = yellow, Mode(s) = orange, Black = density plot, Red = theoretical normal"))) +
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
         caption = (bquote(bar(X)~" displayed as a red dot, Median as a black line, and outlier(s) as small dark red dots"))) +
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
         caption = (bquote(bar(X)~" displayed as a green line, Median as a yellow line, and Mode(s) as orange line(s)"))) +
    geom_histogram(bins=binnumber,color = "black",fill="deepskyblue") +
    geom_vline(xintercept = meanqqq, colour="dark green", linetype="dashed", size=1.5) +
    geom_vline(xintercept = medianqqq, colour="yellow", linetype="dashed", size=1.5) +
    geom_vline(xintercept = modeqqq, colour="orange", linetype="dashed") 

  print(p)
  print(pp)
  print(ppp)
  return(summary(qqq))
} # end function

