MS_K<-function(sp,com,cat=5,by=5,by2=length(unique(sp$FAMILY_CN))-by){

  s1<-MS_K1(sp,com,cat=cat,by=by)
  s2<-MS_K2(sp,com,by=by,by2=by2)
  re1<-paste(s1,s2,sep="")
  re1
}
