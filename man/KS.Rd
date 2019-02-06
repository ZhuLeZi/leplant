\name{KS}
\alias{KS}

\title{
Family and Genus
}
\description{
Analysis of Family and Genus Composition,print the result of arrange by the sum number
}
\usage{
KS(x,by="family")
}

\arguments{
  \item{x}{
sp information data.fram
}
\item{by}{
 by "group","family","genus"
}
}

\examples{
data(plantcom)
sp<-plantcom$sp
KS(sp,by="group")
KS(sp,by="family")
KS(sp,by="genus")
  }

