KS2<-function(sp,by='family'){
sp
  if (by=='group') {
    cs<-sp%>%group_by(GROUP)%>%summarise(k=length(unique(FAMILY_CN)),s=length(unique(GENUS_CN)),z=length(SPECIES))
    names(cs)<-c("类群","科","属","种")
  }
  
  if (by=='family') {
    cs<-sp%>%group_by(FAMILY_CN,FAMILY_APGIII)%>%summarise(s=length(unique(GENUS_CN)),z=length(SPECIES))
    cs<-arrange(cs,-z)
    cs$s1<-(cs$s/sum(cs$s))*100
    cs$s1<-round(cs$s1,2)
    cs$z1<-(cs$z/sum(cs$z))*100
    cs$z1<-round(cs$z1,2)
    names(cs)[3:6]<-c("属数","种数","属所占比（%）","种所占比（%）")
    
  }
  if(by=='genus'){
    cs<-sp%>%group_by(GENUS_CN,GENUS)%>%summarise(z=length(SPECIES))
    cs<-arrange(cs,-z)
    cs$z1<-(cs$z/sum(cs$z))*100
    cs$z1<-round(cs$z1,2)
    names(cs)[3:4]<-c("种数","种所占比（%）")
  }
 cs
}


