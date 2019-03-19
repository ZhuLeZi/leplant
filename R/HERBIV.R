
HERBIV<-function(x,method=3){
  sh<-x

  xdz<-function (x)
  {
    total <- apply(x, 1, sum,na.rm=TRUE)
    x <- sweep(x, 1, total, "/")
    return(x)
  }
  if(method==3.1){
    sp_high<-tapply(sh$hight,list(sh$plot,sh$sp),sum)%>%xdz()
    sp_biomass<-tapply(sh$biomass,list(sh$plot,sh$sp),sum)%>%xdz()
    sp_cover<-tapply(sh$cover,list(sh$plot,sh$sp),sum)%>%xdz()
    plotIV<-(sp_high+sp_biomass+sp_cover)/3}

  if(method==3){
  sp_high<-tapply(sh$hight,list(sh$plot,sh$sp),sum)%>%xdz()
  sp_biomass<-tapply(sh$biomass,list(sh$plot,sh$sp),sum)%>%xdz()
  sp_abun<-tapply(sh$abun,list(sh$plot,sh$sp),sum)%>%xdz()
  plotIV<-(sp_high+sp_biomass+sp_abun)/3}

  if(method==1){
    sp_biomass<-tapply(sh$biomass,list(sh$plot,sh$sp),sum)%>%xdz()
    plotIV<-sp_biomass
  }

  if(method==2){
    sp_biomass<-tapply(sh$biomass,list(sh$plot,sh$sp),sum)%>%xdz()
    sp_high<-tapply(sh$hight,list(sh$plot,sh$sp),sum)%>%xdz()
    plotIV<-(sp_biomass+sp_high)/2
  }


  plotIV<-as.data.frame(plotIV)
  plotiv<-t(plotIV)

  lq <- melt(plotiv,id.vars=c(colnames(plotiv)))
  names(lq)[1:3]<-c("sp","plot","iv")

  lq$site<-word(as.character(lq$plot), 1, sep = fixed('.'))

  la<-tapply(lq$iv,list(lq$site,lq$sp),sum,na.rm=TRUE)


  yfn <- sh%>%group_by(site)%>%summarise(yfn=length(unique(plot)))
  yfn<-yfn[,2]

  newl<-length(la[1,])+1
  la<-as.data.frame(la)
  la[,newl]<-yfn



  la<-as.matrix(la)
  siteiv<- la/la[,newl]
  siteiv<-siteiv[,1:newl-1]



  siteiv[which(siteiv==0)] <- NA
  siteiv

}
