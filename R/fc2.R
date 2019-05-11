fc2<-function(yc,cp2){
  f<-fc(yc,cp2)
  ycc<-paste(yc$site,yc$sumcover,sep="a")%>%unique()%>%as.character()%>%tstrsplit(.,"a")%>%as.data.frame()

  names(ycc)<-c("site","sumcover")
  ycc[,2]<-as.character(ycc$sumcover)%>%as.numeric()
  ycc[,1]<-as.character(ycc$site)%>%as.numeric()


  sumcover<-ycc%>%group_by(site)%>%summarise(cover=mean(sumcover))

  hmax<-f %>% group_by(site)%>%summarise(hmax=max(hmax))


  len<-length(hmax[,1])


  jg2<-hmax[,1]%>%as.data.frame()
  jg2$D<-rep("总盖度",len)
  jg2$hmean<-(hmax$hmax)/2
  jg2$hmin<-rep(0,len)
  jg2$hmax<-hmax$hmax
  jg2$cover<-sumcover$cover
  f2<-as.data.frame(f)
  cs<-rbind(f2,jg2)
  cs
}
