\name{MS_QXDL}
\alias{MS_QXDL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MS_QXDL(sp,com,cut=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{
sp information matrix
}
  \item{com}{
iv matrix
}
  \item{cut}{
the number of print
}
}

\examples{
data(plantcom)
sp<-plantcom$sp
com<-HERBIV(plantcom$comm)
MS_QXDL(sp,com,cut=1)
}
