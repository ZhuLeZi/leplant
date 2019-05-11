subsp<-function(com,by="hyd1",len=20){
  if (by=="hyd1"){
    sj<-HYD(com,out='table')%>%subset(.,'相对频度百分比'>len)
  }
  
  if (by=="hyd"){
    sj<-HYD(com,out='table')[1:len,]
  }
  
  if (by=="iv1"){
    sj<-IVDJ(com,out='table')%>%subset(.,'per'>len)
  }
  
  if (by=="iv"){
    sj<-IVDJ(com,out='table')[1:len,]
  }
  
  sj$sp<-as.character(sj$sp)
  com2<-com%>%as.data.frame()%>%select(., one_of(sj$sp))%>%as.matrix()
  
  com2[is.na(com2)]<-0
  com2
}


