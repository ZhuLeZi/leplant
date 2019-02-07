\name{QLYF}
\alias{QLYF}

\title{
The basic of community description, table
}
\description{
Basis of Community Description List,n: the max number describe,iv:>iv indes will be seem as cp
}
\usage{
QLYF(com,cp,n=5,iv=0.1)
}

\arguments{
  \item{x}{

}
}

\examples{
data(plantcom)
com<-HERBIV(plantcom$comm)
cp<-CPHF(plantcom$sp)
QLYF(com,cp,n=5,iv=0.1)
}

