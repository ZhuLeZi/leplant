MS_K2<-function(sp,com,by=5,by2=length(unique(sp$FAMILY_CN))-by){
  com<-jz_data(com)




  p1<-zhz(sp,com)

  km<-KS(sp,by="k")[1:by,1]%>%as.character()

  mz<-filter(p1, !(FAMILY_CN %in% km))%>%arrange(.,-zh_z)
  mz$ms<-paste(mz$TAXA_NAME,"（",mz$SPECIES,"）",sep="")

  jg1<-tapply(mz$ms,mz$FAMILY_CN,function(x)paste(x,collapse = "、"))%>%as.matrix()

  jg1<-cbind(科名=row.names(jg1),物种组成=jg1)%>%as.data.frame()
  px<-unique(mz$FAMILY_CN)
  px<-cbind(科名=px,科=unique(paste(mz$FAMILY_CN,mz$FAMILY_APGIII,sep="（")),顺序=c(1:length(px)))

  jg2<-merge(px,jg1,by="科名")%>%arrange(.,as.numeric(顺序))

  jg2<-jg2[1:by2,]
  w_z_ms<-paste(jg2$科,"）的",jg2$V2,sep="")%>%paste(.,collapse = ";")
  re<-paste("除以上物种数较多的",by,"个科外，还有",w_z_ms,"也在群落中出现频度较高且优势度较大。",sep='')

  re
}


