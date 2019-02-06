MS_SHX<-function(sp,com){
  ta<-SHX(sp,out="table")
com<-jz_data(com)
  sp$sh<-str_count(sp$生活型,"木")

  nsp<-subset(sp,sp$sh>0)

  shx_ms<-zhz(nsp,com)%>%arrange(.,-zh_z)
  shx_ms$ms<-paste(shx_ms$TAXA_NAME,"（",shx_ms$SPECIES,"）",sep="")

  ms_1<-tapply(shx_ms$ms,shx_ms$生活型,function(x)paste(x,collapse = "、"))%>%as.data.frame()

  ms_1<-cbind(生活型=as.factor(row.names(ms_1)),ms_1)%>%left_join(ta,.,by="生活型")

  msshx_mm<-function(ms_1){
    ms_1$mssh<-paste(ms_1$生活型,"有",ms_1$种数,"种，占总物种数的",ms_1$百分比,"%，有",ms_1$.,sep="")%>%str_replace(., "有NA", "")
    jgms<-paste(ms_1$mssh,collapse = "；")%>%str_replace(., "，；", "；")%>%paste(.,"。",sep="")%>%str_replace(., "，。", "。")
    jgms
  }


  jgms<-msshx_mm(ms_1)


  jgms
}



