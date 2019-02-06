############匹配综合值
zhz<-function(sp,com){
  iv_t<-IVDJ(com,out='table')
  hyd<-HYD(com,out='table')
  qt<-merge(iv_t,hyd,by="sp")

  names(qt)[1]<-"TAXA_NAME"

  p1<-merge(qt,sp,by="TAXA_NAME")

  p1$zh_z<-(p1$频度/sum(p1$频度)*100)+(p1$per)
  p1

}
