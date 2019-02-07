MS_QXDL<-function(sp,com,qxdl,cut=1){
  com<-jz_data(com)
  qxdl<-qxdl
  zh<-zhz(sp,com)%>%merge(.,qxdl,by='区系地理成分')%>%arrange(.,区系地理成分2,-zh_z)

  zh2<-transform(zh,nxh=unlist(tapply(zh$区系地理成分2,zh$区系地理成分2,index)))

  cut<-cut+1

  ms_s<-tapply(zh2$TAXA_NAME,zh2$区系地理成分2,length)%>%as.data.frame()
  ms_s<-cbind(区系地理=row.names(ms_s),ms_s)%>%na.omit()

  zh3<-subset(zh2,zh2$nxh<cut)

  ms_dl<-tapply(zh3$TAXA_NAME,zh3$区系地理成分2,function(x)paste(x,collapse = "、"))%>%as.data.frame()

  ms_dl<-cbind(区系地理=row.names(ms_dl),ms_dl)%>%na.omit()

  qxms_r<-merge(ms_dl,ms_s,by="区系地理")

  qxms_r$per<-qxms_r[,3]/sum(qxms_r[,3])
  names(qxms_r)[2:4]<-c("物种","种数","百分比")
  qxms_r<-arrange(qxms_r,-种数)
  r<-qxms_r
  qxms_r$ms<-paste(r$区系地理,"有",r$种数,"种，占总物种数的",r$百分比,"%，有",r$物种,"等",sep="")
  jg<-paste(qxms_r$ms,collapse = "；")%>%paste(.,"。",sep="")
  jg
}
