\name{MS_QCZ}
\alias{MS_QCZ}

\title{
Community Quantitative Characteristics
}
\description{
Statistical Community Quantitative Characteristics Based on Group Subordinate Groups
}
\usage{
MS_QCZ(comm,cs)
}%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{comm}{
community data 3 plot
}
}

\examples{
com<-HERBIV(plantcom$comm)
cp<-CPHF(plantcom$sp)
cs<-QLYF(com,cp,n=5,iv=0.1)
MS_QCZ(plantcom$comm,cs)
  }
