
###按科描述》生活型》种
MS_K1<-function(sp,com,cat=5,by=5)
{

  k_shx_sum<-function(sp){
    su<-tapply(sp$TAXA_NAME,list(sp$FAMILY_CN,sp$生活型),length)
    su<-cbind(科名=row.names(su),su)%>%as.data.frame()
    jg2<-gather(su,生活型,种数,-科名,na.rm=T)
    jg2
  }

  s<-wz(sp,com,cat=cat)
  su<-k_shx_sum(sp)

  jgll<-merge(s,su,by=c("科名","生活型"))%>%arrange(.,科名,-as.numeric(种数))

  jgll$ms<-paste(jgll$生活型,"有",jgll$种数,"种，常见有",jgll$物种组成,"等",sep="")

  ll<-tapply(jgll$ms,jgll$科名,function(x)paste(x,collapse = "，"))%>%as.data.frame()
  ll<-cbind(科名=row.names(ll),ll)
  k<-KS(sp,by="k")

  l1<-merge(k,ll,by="科名")%>%arrange(.,-as.numeric(种数))
  l2<-l1[1:by,]

  spms<-paste(l2$科名,"（",l2$FAMILY_APG,"），共",l2$属数,"属，",l2$种数,"种，占总物种数的",l2$`种所占比例(%)`,"%，",l2$.,sep="")
  spms<-paste(spms,collapse  = "。")%>%paste(.,"。" ,sep= "")

  spms}



