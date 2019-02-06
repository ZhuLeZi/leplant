#输入数据为cp
TABLE_ZH<-function(cp){
  cs1<-tapply(cp$TAXA_NAME,list(paste(cp$D,cp$FAMILY_CN),cp$水分生态类型),length)%>%as.data.frame()
  shx<-tstrsplit(row.names(cs1),"层")[1]%>%as.data.frame() ##拆分生活型
  ke<-tstrsplit(row.names(cs1)," ")[2]%>%as.data.frame()    ##拆分科

  jg<-data.frame(shx,ke,cs1)
  names(jg)[1]<-"生活型"
  names(jg)[2]<-'科'

  cpsum<-jg[,-(1:2)]
  jg$总计<-apply(cpsum,1,sum,na.rm=T)
  shx_sum<-tapply(jg$总计,jg$生活型,sum)%>%as.data.frame()
  shx_sum<-data.frame(生活型=row.names(shx_sum),生活型物种数=shx_sum[,1])

  jg<-merge(jg,shx_sum,by='生活型')
  jg$百分比<-jg$生活型物种数/sum(jg$总计)*100
  jg$百分比<-round(jg$百分比,2)
  jg<-arrange(jg,生活型,-总计)

  stx_sum<-apply(cpsum,2,sum,na.rm=T)%>%as.data.frame()
  jg$生活型<-as.character(jg$生活型)
  jg$科<-as.character(jg$科)
  jg<-arrange(jg,-jg$生活型物种数)
  jg
}
