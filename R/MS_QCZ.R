QCZ_MS1<-function(t2){
  #群从组记载样地数
  a1<-tapply(t2$site,t2$群从组,length)%>%as.data.frame()
  a1<-cbind(群从组=row.names(a1),a1)
  #
  a2<-tapply(t2$群从序号,t2$群从组,function(x)length(unique(x)))%>%as.data.frame()
  
  a3<-cbind(a1,a2)
  names(a3)[2:3]<-c("样地数","群从数")
  a3
}

QCZ_MS2<-function(herb,t2){
  a<- BHD2(herb,t2,data = "sp",by="qcz")#群从组记载物种数
  b<-BHD2(herb,t2,by='qcz')   #群从组物种饱和度
  c<-BHD2(herb,t2,data = "cover",by="qcz") #群从组盖度
  d<-BHD2(herb,t2,data = "bio",by="qcz")#群从组生物量
  m<-cbind(群从组=row.names(a),a,b,c,d)
  
  m
}

MS_QCZ<-function(herb,cs){
  t2<-QCHF(cs)
  a<-QCZ_MS1(t2)
  b<-QCZ_MS2(herb,t2)
  jg<-left_join(a,b,by="群从组")
  jg
}