\name{QX_TABLE}
\alias{QX_TABLE}

\title{
qxdl table by Inner Mongolia Flora
}
\description{
Distribution Table of Geographical Components
}
\usage{
QX_TABLE(x)
}
\arguments{
  \item{x}{
sp information data.fram
}
}

\examples{
data(plantcom)
sp<-plantcom$sp
data(qx_ecoding)
result<-QX_TABLE(sp,qxdl)

# require(kableExtra),RMD,HTML

c<-which(str_length(result[,1])>2)
kable(result, align = "c") %>%
  kable_styling( full_width = T) %>%
  collapse_rows(columns = c(4,6), valign = "middle")%>%add_indent(c(3:6))

  }
