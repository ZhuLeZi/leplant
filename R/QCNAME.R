QCNAME<-function(com,cp,i=0.1){
  iv<-jz_data(com)%>%subset(.,iv>i)
  cpp<-data.frame(sp=cp$TAXA_NAME,D=cp$D,ld=cp$SPECIES)
  cc<-left_join(iv,cpp,by="sp")
  cc
}


zwqcz<-function(cc){
  qc1<-subset(cc,xh==1)[,2]
  qc2<-subset(cc,xh==2)[,5]
  len<-length(qc2)
  jg <- vector(length = len)
  i<-1
  for (i in i:len) {
    jg[i]<-paste(qc1[i],qc2[i],sep="-")%>%paste(.,"群从组",sep="")
    jg[i]<-paste(qc1[i],qc2[i],sep="-")%>%paste(.,"群从组",sep="")
  }
  jg
}

zwqc<-function(cc){
  a1<-tapply(cc$sp, cc$site, function(x)paste(x, collapse= "-"))%>%paste(.,"群从组",sep="")
  b1<-tapply(cc$ld, cc$site, function(x)paste(x, collapse= "-"))%>%paste("Ass.",.)
  jg<-data.frame(qc=a1,ld=b1,site=unique(cc$site))
  jg
}
