\name{IV_QX}
\alias{IV_QX}

\title{
Table statistical analysis by SUM IV
}
\description{
Table statistical analysis by SUM IV,qxdl,stx(water),stx(bud)
}
\usage{

IV_QX(sp,com)
}

\arguments{
  \item{sp}{
  sp information data.fram
}
\item{com}{
  iv matrix
}
}

\examples{
data(plantcom)
sp<-plantcom$sp
com<-HERBIV(plantcom$comm)
IV_QX(sp,com)

}
