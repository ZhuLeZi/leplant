MS_QC<-function(herb,cs){
  t2<-QCHF(cs)
  a<-BHD2(herb,t2,data = "sp",by="qc")
  
  b<-BHD2(herb,t2,data = "abun",by="qc")
  
  c<-BHD2(herb,t2,data = "cover",by="qc")
  
  d<-BHD2(herb,t2,data = "bio",by="qc")
  
  m<-cbind(群从=row.names(a),a,b,c,d)
  
  m
}
