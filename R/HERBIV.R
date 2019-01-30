
HERBIV<-function(x){
  sh<-x
  sp_high<-tapply(sh$hight,list(sh$plot,sh$sp),sum)
  sp_biomass<-tapply(sh$biomass,list(sh$plot,sh$sp),sum)
  sp_abun<-tapply(sh$abun,list(sh$plot,sh$sp),sum)
  #
  xdz<-function (x)
  {
    total <- apply(x, 1, sum,na.rm=TRUE)
    x <- sweep(x, 1, total, "/")
    return(x)
  }
  xdsp_high<-xdz(sp_high)
  xdsp_abun<-xdz(sp_abun)
  xdsp_biomass<-xdz(sp_biomass)
  #
  plotIV<-(xdsp_high+xdsp_abun+xdsp_biomass)/3
  plotIV<-as.data.frame(plotIV)

  plotiv<-t(plotIV)

  lq <- melt(plotiv,id.vars=c(colnames(plotiv)))
  names(lq)[1:3]<-c("sp","plot","iv")

  lq$site<-word(as.character(lq$plot), 1, sep = fixed('.'))

  la<-tapply(lq$iv,list(lq$site,lq$sp),sum,na.rm=TRUE)
  siteiv<-la/3
  siteiv[which(siteiv==0)] <- NA
  siteiv

}
