\name{MS_K}
\alias{MS_K}

\title{
describe by family
}
\description{
Description by dominant families and lifestyles

}
\usage{
MS_K(sp,com,cat=5,by=5,by2=length(unique(sp$FAMILY_CN))-by
}

\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}

\examples{
data(plantcom)
sp<-plantcom$sp
com<-HERBIV(plantcom$comm)
MS_K(sp,com,cat=5,by=5,by2=5)
  }
