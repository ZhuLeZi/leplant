QCHF<-function(x){
  qc<-x
  df<-qcmm(qc)%>%qcpx()
  df
}

QC_NAME<-function(x,by='sp'){

  ql<-x
  ql<-subset(ql,cp=="层片")

  if (by=='sp') {
    qca<-tapply(ql$sp,ql$site,function(x)paste(x,collapse="-"))
    qcb<-str_c(qca,'群从')
  }
  if (by=='ld') {
    qca<-tapply(ql$ld_name,ql$site,function(x)paste(x,collapse="-"))
    qcb<-str_c('Ass. ',qca)
  }
  qcb<-as.data.frame(qcb)

  if (by=='sp'){
    names(qcb)<-"群从"
  }

  if (by=='ld'){
    names(qcb)<-"Ass."
  }

  qcb
}


QCZ_NAME<-function(x,by='sp'){
  ql<-x

  qc1<-ql[,c(2,1,4)]%>%subset(.,ql$D_xh==1)
  qc2<-ql[,c(2,3,4)]%>%subset(.,ql$D_xh==2)
  names(qc1)<-c("site","D","D_xh")

  qc_1<-ql[,c(2,11,4)]%>%subset(.,ql$D_xh==1)
  qc_2<-ql[,c(2,3,4)]%>%subset(.,ql$D_xh==2)
  names(qc_1)<-c("site","D","D_xh")

  if (by=='sp') {
    qc3<-rbind(qc1,qc2)
    qc3<-arrange(qc3,site,D_xh)
    qcz<-tapply(qc3$D,qc3$site,function(x)paste(x,collapse="-"))%>%str_c(.,'群从组')

  }

  if (by=='ld') {
    qc3<-rbind(qc_1,qc_2)%>%arrange(.,site,D_xh)
    qcz<-tapply(qc3$D,qc3$site,function(x)paste(x,collapse="-"))%>%str_c('Ass. Group ',.)
  }
  qcz<-as.data.frame(qcz)

  if (by=='sp'){
    names(qcz)<-"群从组"
  }

  if (by=='ld'){
    names(qcz)<-"Ass.Group"
  }
  qcz
}

qcmm<-function(x){
  qc<-x
  qc1<-QC_NAME(qc)
  qc11<-QC_NAME(qc,by='ld')
  qcz2<-QCZ_NAME(qc)
  qcz22<-QCZ_NAME(qc,by='ld')

  qc_t<-cbind(qc1,qc11,qcz2,qcz22)
  qc_t$site<-row.names(qc_t)
  qc_t
}




LM<-function(x){
  xh<-x
  px<-c(
    "1"<-'Ⅰ',
    "2"<-'Ⅱ',
    "3"<-'Ⅲ',
    "4"<-'Ⅳ',
    "5"<-'Ⅴ',
    "6"<-'Ⅵ',
    "7"<-'Ⅶ',
    "8"<-'Ⅷ',
    "9"<-'Ⅸ',
    "10"<-'Ⅹ',
    "11"<-'Ⅺ',
    "12"<-'Ⅻ'
  )

  i<-1
  total<-length(xh)
  D <- vector(length = total)
  for(i in i:total){D<-px[xh]
  }
  D
}






#群从排序，命名
qcpx<-function(x){
  qc_t<-x
  df<-arrange(qc_t,群从组)
  df<-transform(qc_t, 群从组序号=as.integer(qc_t$群从组))%>%arrange(.,群从组序号,群从)
  #组内从大到小编号
  df<-transform(df,群从序号=unlist(tapply(群从组序号,qc_t$群从组,index)))
  df
}

QCTABLE<-function(x,out='table'){
  css<-QCHF(x)
  css$群从组序号<-LM(css$群从组序号)
  if(out=='mm'){
    qc<-data.frame(群从组=paste(css$群从组序号,".",css$群从组,sep=""),群从=paste(css$群从组序号,"-",css$群从序号,css$群从,sep=""),ASS=css$Ass.,site=css$site)
  }
  if(out=='table'){
    qc<-data.frame(群从组=paste(css$群从组序号,".",css$群从组),群从=paste(css$群从组序号,"-",css$群从序号,css$群从))
  }
  qc
}
