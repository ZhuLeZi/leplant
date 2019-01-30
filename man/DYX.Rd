\name{DYX}
\alias{DYX}

\title{
diversity
}
\description{
calculate diversity index
}
\usage{
DYX(iv)
}

\arguments{
  \item{iv}{
iv matrix
}
}

\examples{
data(plantcom)
iv<-HERBIV(plantcom$comm)
DYX(iv)

  }

