


########################################
QLYF<-function(com,cp,n=5,iv=0.1){
  QL<-function(com,cp,n=5){
    com<-jz_data(com)
    cp<-cp
    cp<-data.frame(sp=cp$TAXA_NAME,D=cp$D)
    cat<-n+1
    new_com<-merge(com,cp,by = 'sp')

    ####按层片，物种数总数
    sum_cp<-tapply(new_com$sp,list(new_com$site,new_com$D),length)%>%jz_data()
    names(sum_cp)<-c("site","D","物种数","xh")
    sum_cp<-sum_cp[,-4]
    ####按层片，重要值总数
    iv_cp<-tapply(new_com$iv, list(new_com$site,new_com$D), sum)%>%jz_data()
    names(iv_cp)<-c("site","D","D_iv","xh")
    ###层片内最大的IV值
    max_cp_sp<-tapply(new_com$iv,list(new_com$site,new_com$D),max)%>%jz_data()
    names(max_cp_sp)<-c("site","D","iv","xh")

    cs<-merge(max_cp_sp,new_com,by=c("site","D","iv"))%>%merge(.,iv_cp,by=c("site","D")) %>%merge(.,sum_cp,by=c("site","D"))

    cs<-data.frame(site=cs$site,D=cs$D,D_xh=cs$xh,D_iv=cs$D_iv,物种数=cs$物种数,sp=cs$sp,sp_iv=cs$iv,sp_iv=cs$xh.y)%>%arrange(.,site,D_xh)

    #new_com<-arrange(new_com,site,xh)


    newcom1<-new_com
    newcom1$cs<-paste(newcom1$site,newcom1$D)
    newcom1<-arrange(newcom1,site,cs,xh)
    newcom1<-transform(newcom1,nxh=unlist(tapply(cs,cs,index)))

    #newcom2<-subset(newcom1,nxh<cat)
    newcom2<-filter(newcom1,nxh<cat)
    newcom2<-arrange(newcom2,site,xh)

    cx<-tapply(newcom2$sp,list(newcom2$site,newcom2$D),function(x)paste(x,collapse="、"))%>%as.data.frame()
    cx<-cbind(site=row.names(cx), cx)

    ca<-melt(cx,id.vars=c("site"),variable.name="D",
             value.name="物种",na.rm = TRUE)

    re<-merge(cs,ca,by=c("site","D"))%>%arrange(.,site,D_xh)

    re
  }


  #层片划分，X为QL后结果，y为划分级别IV，将大于或等于此数值的构成层片，小于此值则认为不构成层片。
  QCUT<-function(x,iv=0.1){
    cs<-x
    cs[,2]<-tstrsplit(cs$D,"层")[1]

    cs$cp<-cs$D_iv

    cs$cp[which(cs$cp>iv|cs$cp==iv)] ="层片"
    cs$cp[which(cs$cp<iv)] ="非"
    cs
  }

  PP<-function(qc,cp){
    qc<-qc
    sp<-cp

    data<-data.frame(sp=sp$TAXA_NAME,ld_name=sp$SPECIES)
    jg<-merge(qc,data,by=c("sp"))
    jg<-arrange(jg,site,D_xh)
    jg
  }


  n=n
  iv=iv
  jg<-QL(com,cp,n=n)%>%QCUT(.,iv=iv)%>%PP(.,cp)
  jg
}



