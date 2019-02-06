IV_QX<-function(sp,com){
  com<-jz_data(com)
  z<-zhz(sp,com)

  a<- group_by(z,区系地理成分)%>%summarise(.,sum= n())%>%arrange(.,-sum)

  b<- group_by(z,水分生态类型)%>%summarise(.,sum=n())%>%arrange(.,-sum)

  c<- group_by(z,生活型2)%>%summarise(.,sum=n())%>%arrange(.,-sum)

  jg<-list(区系地理成分=a,水分生态类型=b,生活型2=c)
  jg
}
