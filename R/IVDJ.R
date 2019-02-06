IVDJ<-function(com,out='table'){
  hyd<-jz_data(com)
  hy<-tapply(hyd$iv,hyd$sp,sum)%>%as.data.frame()
  hyd<-data.frame(sp=row.names(hy),iv=hy[,1])%>%arrange(.,-iv)
  hyd$per<-(hyd$iv/sum(hyd$iv))*100
  hyd$dj<-cut(hyd$per,c(0,20,40,60,80,100))

  spp<-table(hyd$dj)%>%as.data.frame()
  spp$per<-spp$Freq/sum(spp$Freq)*100
  spp$per<-round(spp$per,2)
  if(out=='table'){
    jg<-hyd
  }

  if(out=='plot'){
    p<-pot(spp)
    p<-p+labs(x="重要值等级 Important value class")
    jg<-p
  }
  jg

}
