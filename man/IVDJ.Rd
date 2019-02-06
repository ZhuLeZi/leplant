\name{IVDJ}
\alias{IVDJ}

\title{
importance value class
}
\description{
print importance value class
}
\usage{
IVDJ(x,out="plot")
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
IVDJ(com,out='table')
IVDJ(com,out='plot')
  }

