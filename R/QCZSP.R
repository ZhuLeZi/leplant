SUBSP<-function(herb,cs){
  t2<-QCHF(cs)
  t2m<-data.frame(site=t2$site,群从组=t2$群从组,群从=t2$群从)
  ll<-merge(herb,t2m,by="site")
  
  subsp<-tapply(ll$sp,ll$群从组,unique)
  
  subsp
}



QCSP<-function(sp,y){
  sp_qc<-y
  
  sp1<-as.data.frame(sp_qc)
  names(sp1)<-"TAXA_NAME"
  
  sp2<-merge(sp1,sp,by="TAXA_NAME")
  sp2
}

QCZSP<-function(herb,cs,sp){
  su<-SUBSP(herb,cs)

jgg<-list()
i=1
for (i in i:length(su)) {
 
  jgg[[i]]<- QCSP(sp,su[i])
}
names(jgg)<-names(su)

jgg
}