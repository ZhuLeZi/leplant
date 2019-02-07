SHX<-function(sp,by='shx',out='plot',qxdl=NA){
  spp<-sp
  qxdl<-qxdl
  if (by=='shx') {
    spp<-table(spp$生活型)%>%as.data.frame()
    spp$per<-spp$Freq/sum(spp$Freq)*100
    spp$per<-round(spp$per,2)
    if(out=='plot'){
      spp$Var1<-factor(spp$Var1,levels = c("灌木","小灌木","半灌木","小半灌木","多年生草本","一年生草本","一、二年生植物","草质藤本"),ordered=T)
      p<-pot(spp)
      p<-p+labs(x="生活型 Life form")
    }
    if(out=='table'){
      names(spp)<-c("生活型","种数","百分比")
      p<-arrange(spp,-百分比)
    }
  }
  if (by=='shx2') {
    spp<-table(spp$生活型2)%>%as.data.frame()
    spp$per<-spp$Freq/sum(spp$Freq)*100
    spp$per<-round(spp$per,2)
    if(out=='plot'){
      spp$Var1<-factor(spp$Var1,levels = c("高位芽植物","地上芽植物","地面芽植物","地下芽植物","一年生植物"),ordered=T)
      p<-pot(spp)
      p<-p+labs(x="生活型 Life form")
    }
    if(out=='table'){
      names(spp)<-c("生活型","种数","百分比")
      p<-arrange(spp,-百分比)
    }
  }
  if (by=='stx') {
    spp<-table(spp$水分生态类型)%>%as.data.frame()
    spp$per<-spp$Freq/sum(spp$Freq)*100
    spp$per<-round(spp$per,2)

    if(out=='plot'){
      spp$Var1<-factor(spp$Var1,levels = c("超旱生","强旱生","旱生","中旱生","中生","旱中生","湿中生","湿生"),ordered=T)
      p<-pot(spp)
      p<-p+labs(x="植物水分生态类型 Plant water ecotypes")
    }
    if(out=='table'){
      names(spp)<-c("水分生态类型","种数","百分比")
      p<-arrange(spp,-百分比)
    }
  }

  if (by=='qxdl') {
    spp<-table(spp$区系地理成分)%>%as.data.frame()
    spp$per<-spp$Freq/sum(spp$Freq)*100
    spp$per<-round(spp$per,2)
    if(out=='plot'){
      p<-pot(spp)
      p<-p+labs(x="植物区系地理成分 Floristic geographic element")
    }
    if(out=='table'){
      names(spp)<-c("区系地理成分","种数","百分比")
      p<-arrange(spp,-百分比)
    }}

  if (by=='qxdl2') {

    spp<-merge(sp,qxdl,by='区系地理成分')
    spp<-table(spp$区系地理成分2)%>%as.data.frame()
    spp$per<-spp$Freq/sum(spp$Freq)*100
    spp$per<-round(spp$per,2)
    spp<-subset(spp,spp$Freq > 0)
    if(out=='plot'){
      spp$Var1<-factor(spp$Var1,levels = as.character(unique(qxdl$区系地理成分2)),ordered=T)
      p<-pot(spp)
      p<-p+labs(x="植物区系地理成分 Floristic geographic element")
    }
    if(out=='table'){
      names(spp)<-c("区系地理成分","种数","百分比")
      p<-arrange(spp,-百分比)
    }
  }
  p
}
