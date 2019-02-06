\name{HYD}
\alias{HYD}

\title{
HYD：Constance class
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
HYD(x,out="plot")
}

\arguments{
  \item{x}{
iv matrix
}
  \item{out}{
out print style "plot","table"
}
}

\examples{
data(plantcom)
com<-HERBIV(plantcom$comm)
HYD(com,out='table')
HYD(com,out='plot')
  }

