QCHF2<-function(x){
  qc<-x
  qc_t<-qcmm(qc)
  ####删除组内重复值
  cs<-tapply(qc_t$site, qc_t$群从,function(x)paste(x,collapse ="、"))%>%as.data.frame()
  cs1<-data.frame(样地号=cs[,1],群从=row.names(cs))
  jg<-merge(qc_t,cs1,by="群从")

  df<-jg[!duplicated(jg$群从),]%>%qcpx()

  df
}
#和新的QCHF结果对应
###x=t1
QCTABLE2<-function(x,data='qc',out='table'){
  css<-QCHF2(x)
  css$群从组序号<-LM(css$群从组序号)

  if(data=='yf'){
    if(out=='mm'){
      qc<-data.frame(群从组=paste(css$群从组序号,".",css$群从组,sep=""),群从=paste(css$群从组序号,"-",css$群从序号,css$群从,sep=""),Ass=css$Ass.,site=css$site)
      #可以不要下一行，可以输出data.fram
      #qc<-paste(qc$群从,qc$Ass,qc$site)
    }
    if(out=='table'){
      qc<-data.frame(群从组=paste(css$群从组序号,".",css$群从组),群从=paste(css$群从组序号,"-",css$群从序号,css$群从))
    }}

  if(data=='qc'){

    if(out=='mm'){
      qc<-paste(paste(css$群从组序号,"-",css$群从序号,sep=""),css$群从,css$Ass.,paste("(",css$样地号,")",sep=""))
    }
    if(out=="table"){
      qc<-data.frame(群从组=paste(css$群从组序号,".",css$群从组,sep=""),群从=paste(css$群从组序号,"-",css$群从序号,css$群从,sep=""))
    }
  }
  qc
}


