\name{QLYF}
\alias{QLYF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
QLYF(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}

\examples{
data(plantcom)
com<-HERBIV(plantcom$comm)
cp<-CPHF(plantcom$sp)
QLYF(com,cp,n=5,iv=0.1)
}

