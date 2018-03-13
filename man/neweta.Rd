\name{neweta}
\alias{neweta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates eta squared for an AOV model using the Type II method
}
\description{
Takes an aov object and returns a standard AOV table with eta squared computed
}
\usage{
neweta(MyAOV)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MyAOV}{a valid aov object such as those produced by \code{aov(dv~iv1*iv2)}}
  }

\details{
There are three ways to compute eta squared this function only uses Type II
}
\value{
Returns a tibble containing the AOV output similar to \code{summary(aov(MyAOV))} but with eta squared computed and appended as an additional column
}
\references{
neweta function is a shortened and simplified verion of Dani Navarro's \code{\link[lsr]{etaSquared}}
}
\author{
Chuck Powell
}

\seealso{
\code{\link{Plot2WayANOVA}}
}
\examples{
neweta(aov(mpg~am*cyl, mtcars))
}