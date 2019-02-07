\name{MS_QC}
\alias{MS_QC}
\title{
ASS.group Quantitative characteristics,cover abun biomass
}
\description{
Statistical Quantitative Characteristics by Groups
}
\usage{
MS_QC(herb,cs)
}

\examples{
data(plantcom)
com<-HERBIV(plantcom$comm)
cp<-CPHF(plantcom$sp)
cs<-QLYF(com,cp,n=5,iv=0.1)
MS_QC(plantcom$comm,cs)
}

