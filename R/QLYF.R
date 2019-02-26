


########################################
QLYF<-function(com,cp,n=5,iv=0.1){

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



