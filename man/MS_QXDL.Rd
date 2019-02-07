\name{MS_QXDL}
\alias{MS_QXDL}
\title{
Description by qxdl
}
\description{
Description according to floristic geographical elements
}
\usage{
MS_QXDL(sp,com,qxdl,cut=1)
}

\arguments{
  \item{sp}{
sp information matrix
}
  \item{com}{
iv matrix
}
\item{qxdl}{
qxdl data.fram
}
  \item{cut}{
the number of print
}
}

\examples{
data(plantcom)
data(qx_ecoding)
sp<-plantcom$sp
com<-HERBIV(plantcom$comm)
MS_QXDL(sp,com,qxdl,cut=1)
}
