
CORB<-function(da,by="pearson"){

  nc=length(da)
  tj<-rcorr(as.matrix(da),type = by)

  xgx<-as.character(round(tj$r,3))

  xz<-tj$P
  xz[is.na(xz)] <- 1
  xzx<-symnum(xz,cutpoints = c(0,0.01,0.05,1),symbols = c("**","*",""))%>%as.character()



  jg<-paste(xgx,xzx,sep="")%>%matrix(.,ncol=nc,byrow=T)%>%as.data.frame()

  names(jg)<-names(da)
  row.names(jg)<-names(da)
  jg
}
