fc<-function(herb,cp){
  cp2<-data.frame(sp=cp$TAXA_NAME,D=cp$D)
cp2$sp<-as.character(cp2$sp)
  herb2<-left_join(herb,cp2,by="sp")
  fc<-herb2%>%group_by(site,D)%>%summarise(hmin=min(hight),hmax=max(hight),cover=sum(cover)/3)
}