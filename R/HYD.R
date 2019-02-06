###iv =HERBIV(plantcom$comm)
HYD<-function(iv,out='table'){
  com<-jz_data(iv)
  hyd<-com
  hy<-tapply(hyd$sp,hyd$sp,length)%>%as.data.frame()
  hyd<-data.frame(sp=row.names(hy),频度=hy[,1])%>%arrange(.,-频度)
  hyd$相对频度百分比<-(hyd$频度/max(com$site))*100
  hyd$dj<-cut(hyd$相对频度百分比,c(0,20,40,60,80,100))

  spp<-table(hyd$dj)%>%as.data.frame()
  spp$per<-spp$Freq/sum(spp$Freq)*100
  spp$per<-round(spp$per,2)
  if(out=='table'){
    jg<-hyd
  }

  if(out=='plot'){
    p<-pot(spp)

    p<-p+labs(x="恒有度等级 Constance class")
    jg<-p
  }
  jg

}

