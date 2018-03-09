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
  \item{MyAOV}{a valid aov object such as those produced by aov(dv~iv*iv)}
  }

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns a tibble containing the AOV output similar to summary(aov(MyAOV)) but with eta squared conputed
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
neweta(aov(mpg~am*cyl, mtcars))
}