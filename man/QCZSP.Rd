\name{QCZSP}
\alias{QCZSP}

\title{
filter sp by Ass.groub
}
\description{
filter sp by Ass.groub for KS Analysis
}
\usage{
QCZSP(herb,cs,sp)
}

\examples{
data(plantcom)
com<-HERBIV(plantcom$comm)
cp<-CPHF(plantcom$sp)
cs<-QLYF(com,cp,n=5,iv=0.1)

QCZSP(plantcom$comm,cs,sp)
}
