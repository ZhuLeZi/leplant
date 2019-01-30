jz_data<-function(x){
  siteiv<-x
  lv<-melt(siteiv,id.vars=c(colnames(siteiv)))
  names(lv)[1:3]<-c("site","sp","iv")
  lo<-na.omit(lv)
  lo<-arrange(lo,site,-iv)

  d<-tapply(lo$sp,lo$site,function(x)rep(1:length(x)))
  lo$xh = as.vector(unlist(d[1:max(lo$site)]))
  lo
}
