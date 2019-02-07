\name{SHX}
\alias{SHX}

\title{
 print the description of the Flora Life form
}
\description{
print shx(growth type),shx2(Bud position),stx(Water growth type),qxdl,qxdl2( division by Inner Mongolia Flora)
}
\usage{
SHX(x,by="shx",out="plot")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
sp species information data.fram
}
 \item{by}{
Describing topics
}
 \item{out}{
the style of result (plot,table)
}
}


\examples{
data(plantcom)
sp<-plantcom$sp

SHX(sp,by="shx",out='plot')
SHX(sp,by="shx",out='table')
SHX(sp,by="qxdl2",out='table',qxdl=qxdl)
}

