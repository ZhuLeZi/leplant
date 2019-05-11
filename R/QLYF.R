
PP<-function(qc,cp){
  qc<-qc
  sp<-cp

  data<-data.frame(sp=sp$TAXA_NAME,ld_name=sp$SPECIES)
  jg<-merge(qc,data,by=c("sp"))
  jg<-arrange(jg,site,D_xh)
  jg
}

#层片划分，X为QL后结果，y为划分级别IV，将大于或等于此数值的构成层片，小于此值则认为不构成层片。
QCUT<-function(x,iv=0.1){
  cs<-x
  cs[,2]<-tstrsplit(cs$D,"层")[1]
  cs$cp<- ifelse(cs$D_iv<iv, "非", "层片")
  cs
}

#层片划分,根据盖度
QCUT2<-function(x,cover=10){
  cs<-x
  cs[,2]<-tstrsplit(cs$D,"层")[1]
  cs$cp<- ifelse(cs$cover<cover, "非", "层片")
  cs
}


########################################
QLYF<-function(com,cp,n=5,iv=0.1){
  n=n
  iv=iv
  jg<-QL(com,cp,n=n)%>%QCUT(.,iv=iv)%>%PP(.,cp)
  jg
}


##

QLYF2<-function(com,cp,herb,n=5,cover=10){
  n=n
  cover=cover

  jg<-QL(com,cp,n=n)
  jg$pp<-paste(jg$site,jg$D)
  f<-fc(herb,cp)%>%as.data.frame()
  f$pp<-paste(f$site,f$D)
  f2<-f[,-c(1:5)]
  cx2<-left_join(jg,f2,by="pp")%>%QCUT2(.,cover=cover)%>%PP(.,cp)
  cx2
}
