\name{Plot2WayANOVA}
\alias{Plot2WayANOVA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a 2 Way ANOVA using dplyr and ggplot2
}
\description{
Takes a formula and a dataframe as input, conducts an analysis of variance and produces the results (AOV summary table and table of means) to the console and as a plotted interaction graph using ggplot2.
}
\usage{
Plot2WayANOVA(formula, dataframe = NULL, confidence=.95, plottype = "bar")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a valid R formula with a numeric dependent (outcome) variable, and two independent (predictor) variables e.g. mpg~am*vs. The independent variables are forced to factors (with warning) if possible.}
  \item{dataframe}{a valid R dataframe}
  \item{confidence}{what confidence level for confidence intervals}
  \item{plottype}{bar or line (quoted)}
  }

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
An object of class 'ggplot' which can be saved for futre use.  The most interesting sub element in the list is $data whuch contains the means table for the dependent variable grouped by independent variables.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chuck Powell
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
Plot2WayANOVA(mpg~am*cyl, mtcars, plottype = "line")
Plot2WayANOVA(mpg~am*vs, mtcars, confidence = .99)
}