\name{MS_SHX}
\alias{MS_SHX}

\title{
Lifestyle Description
}
\description{
Lifestyle Description, Herbal Statistics, Woody List Species
}
\usage{
MS_SHX(sp,com)
}
\arguments{
  \item{sp}{
sp information data.fram
}
  \item{com}{
iv  matrix
}
}

\examples{
data(plantcom)
sp<-plantcom$sp
com<-HERBIV(plantcom$comm)

MS_SHX(sp,com)
  }

