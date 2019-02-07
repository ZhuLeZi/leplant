\name{CPMS}
\alias{CPMS}
\title{
Describing species composition
}
\description{
Describing species composition,iv ,number,
}
\usage{
CPMS(cs,by="zw")
}

\examples{
data(plantcom)
com<-HERBIV(plantcom$comm)
cp<-CPHF(plantcom$sp)
cs<-QLYF(com,cp,n=5,iv=0.1)
CPMS(cs,by="cp")
CPMS(cs,by="zw")
}
