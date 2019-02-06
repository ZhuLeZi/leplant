MS_STX<-function(sp){
  mt<-SHX(sp,by='stx',out='table')
  mt$ms<-paste(mt$水分生态类型,"植物有",mt$种数,"种，占总物种数的",mt$百分比,"%",sep="")
  jg<-paste(mt$ms,collapse = "；")%>%paste(.,"。",sep = "")
  jg
}
