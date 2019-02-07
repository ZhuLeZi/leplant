\name{QCTABLE2}
\alias{QCTABLE2}

\title{
ASS. divide and name
}
\description{

}
\usage{
QCTABLE2(cs, data = "qc", out = "table")
}

\arguments{
  \item{cs}{
basic analysis of community table
}
  \item{data}{
by qc or yf
}
  \item{out}{

}
}

\examples{
data(plantcom)
com<-HERBIV(plantcom$comm)
cp<-CPHF(plantcom$sp)
cs<-QLYF(com,cp,n=5,iv=0.1)
QCTABLE2(cs,out='table')
QCTABLE2(cs,out='mm')
}
