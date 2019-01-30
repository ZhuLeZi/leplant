TH<-function(sp,text){

  s1<-sp$TAXA_NAME
  s<-s<-paste(sp$TAXA_NAME,sp$SPECIES,sep="(")%>%paste(.,")",sep="")

  t<-length(s1)
  i<-1
  for(i in i:t) {
    text<-str_replace(text,s1[i],s[i])
  }
  text

}
