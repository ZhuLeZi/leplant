
#按照物种频度，根据科，生活型，排列
wz<-function(sp,com,cat=5){
  com<-jz_data(com)
  ###最多描述物种数
  SUB_SP<-function(sp,cat=5){
    cat=cat
    sp$cs<-paste(sp$FAMILY_CN,sp$生活型)
    newcom1<-arrange(sp,sp$FAMILY_CN,sp$生活型,-sp$频度)
    newcom1<-transform(newcom1,nxh=unlist(tapply(cs,cs,index)))

    sp_jg<-subset(newcom1,newcom1$nxh<cat)
    sp_jg
  }

  c_s<-HYD(com,out='table')[,1:2]
  names(c_s)<-c("TAXA_NAME","频度")
  #
  sp<-merge(c_s,sp,by="TAXA_NAME")
  sp<-arrange(sp,sp$FAMILY_CN,sp$生活型,-sp$频度)
  #物种组成
  cat<-cat+1
  sp<-SUB_SP(sp,cat=cat)

  sp$aa<-paste(sp$TAXA_NAME,sp$SPECIES,sep = "(")%>%paste(.,")",sep = "")
  a<-tapply(sp$aa,list(sp$FAMILY_CN,sp$生活型),function(x)paste(x,collapse="、"))
  a<-cbind(科名=row.names(a),a)%>%as.data.frame()
  jg1<-gather(a,生活型,物种组成,-科名,na.rm=T)
  jg1
}


