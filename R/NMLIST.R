NMLIST<-function(herb){

  sp<-unique(herb$sp)%>%as.data.frame()
  names(sp)<-"TAXA_NAME"
  sp$TAXA_NAME<-as.character(sp$TAXA_NAME)
  spp<-leplant::spp
  jg<-left_join(sp,spp,by="TAXA_NAME")
  jg
}
