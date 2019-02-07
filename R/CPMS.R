CP1<-function(tcp){
  a<-paste(tcp$D,tcp$cp,"，","累计重要值为",round(tcp$D_iv,3),"，"
           ,tcp$数,tcp$物种数,"种，","优势种为",tcp$sp,"，","重要值为",round(tcp$sp_iv,3),"，","还有",tcp$物种,"等",
           sep="")%>%str_replace(., "NA", "")
  a
}

CP2<-function(tzw){
  a<-paste(tzw$D,"，","累计重要值为",round(tzw$D_iv,3),"，"
           ,tzw$数,tzw$物种数,"种，",'有',tzw$物种,
           sep="")%>%str_replace(., "NA", "")
  a
}

CPMS<-function(x,by="cp"){
  t1<-x
  t1$数<-'记有'
  t1$数[which(t1$物种数==1)]<-'仅'
  
  
  if(by=='cp'){
    tcp<-subset(t1,t1$cp=="层片")
    tcp$物种<-str_replace(as.character(tcp$物种),as.character(tcp$sp), "")
    tcp$物种<-str_replace(as.character(tcp$物种),'、', "")
    
    tcp$cpms<-CP1(tcp)%>%str_replace(., "，还有等", "")%>%str_replace(., "1种，", "")
    mscp<-tapply(tcp$cpms,tcp$site,function(x)paste(x,collapse  = ";"))%>%as.data.frame()
  }
  if(by=='zw'){
    c_p<-"层片"
    tzw<- filter(t1, !(t1$cp %in% c_p))#对非层片植物进行描述
    ta<-tzw
    ta$cpms<-CP2(ta)%>%str_replace(., "1种，", "")
    mscp<-tapply(ta$cpms,ta$site,function(x)paste(x,collapse  = ";"))%>%as.data.frame()
  }
  mscp
}
