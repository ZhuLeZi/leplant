
  QX_TABLE<-function(sp){
    qxdl=leplant::qx_ecoding
    sp<-sp
    cs<-tapply(sp$TAXA_NAME,sp$区系地理成分,length)%>%as.data.frame()
    cs<-data.frame(区系地理成分=row.names(cs),种数=cs$.)
    cs1<-merge(cs,qxdl,by='区系地理成分')

    n<-tstrsplit(cs1$序号,"[.]")[1]%>%as.data.frame()
    names(n)<-'xh'

    cs2<-data.frame(cs1,n)
    cs2$xh<-as.character(cs2$xh)%>%as.numeric()
    cs3<-arrange(cs2,cs2$xh,cs2$序号)
    cs3$百分比<-((cs3$种数/sum(cs3$种数))*100)%>%round(.,2)

    cs4<-tapply(cs3$种数,cs3$区系地理成分2,sum)%>%as.data.frame()

    cs4<-cbind(区系地理成分2=row.names(cs4),cs4,per=(cs4$./sum(cs4,na.rm = T))*100)

    cs5<-left_join(cs3,cs4,by='区系地理成分2')

    jg<-data.frame(序号=cs5$序号,区系地理成分=cs5$区系地理成分,种数=cs5$种数,合计种数=cs5$.,百分比=cs5$百分比,合计百分比=cs5$per)

    jg$合计百分比<-round(jg$合计百分比,2)
    jg

  }
