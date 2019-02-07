BHD2<-function(herb,t2,data='abun',by='qc'){
  
  m1<-merge(herb,t2,by='site')
  if(data=='abun'){
    bh<-tapply(m1$plot,m1$plot,length)%>%as.data.frame()
    bh$plot<-row.names(bh)
    m2<-merge(m1,bh,by='plot')
    
    if(by=='qc'){
      jg<-tapply(m2$.,m2$群从,function(x)paste(min(x),"~",max(x)))%>%as.data.frame()
      jg$mean<-tapply(m2$.,m2$群从,mean)%>%round(.,0)
    }
    
    if(by=='qcz'){
      jg<-tapply(m2$.,m2$群从组,function(x)paste(min(x),"~",max(x)))%>%as.data.frame()
      jg$mean<-tapply(m2$.,m2$群从组,mean)%>%round(.,0)
    }
    
    if(by=='yf'){
      jg<-tapply(m2$.,m2$site,function(x)paste(min(x),"~",max(x)))%>%as.data.frame()
      jg$mean<-tapply(m2$.,m2$site,mean)%>%round(.,0)
    }
    jg<-as.data.frame(jg)
    names(jg)<-c('每平方米物种饱和度','平均物种数')
  }
  
  if(data=='cover'){
    cov<-tapply(m1$sumcover,m1$plot,max)%>%as.data.frame()
    cov$plot<-row.names(cov)
    m2<-merge(m1,cov,by='plot')
    if(by=='qc'){
      jg<-tapply(m2$.,m2$群从,function(x)paste(min(x),"~",max(x)))%>%as.data.frame()
      jg$mean<-tapply(m2$.,m2$群从,mean)%>%round(.,2)
    }
    if(by=='qcz'){
      jg<-tapply(m2$.,m2$群从组,function(x)paste(min(x),"~",max(x)))%>%as.data.frame()
      jg$mean<-tapply(m2$.,m2$群从组,mean)%>%round(.,2)
    }
    if(by=='yf'){
      jg<-tapply(m2$.,m2$site,function(x)paste(min(x),"~",max(x)))%>%as.data.frame()
      jg$mean<-tapply(m2$.,m2$site,mean)%>%round(.,2)
    }
    jg<-as.data.frame(jg)
    names(jg)<-c('每平方米群落总盖度','平均盖度')
  }
  
  if(data=='sp'){
    if(by=='qc'){
      jg<-tapply(m1$sp,m1$群从,function(x)length(unique(x)))%>%as.data.frame()
    }
    if(by=='qcz'){
      jg<-tapply(m1$sp,m1$群从组,function(x)length(unique(x)))%>%as.data.frame()
    }
    if(by=='yf'){
      jg<-tapply(m1$sp,m1$site,function(x)length(unique(x)))%>%as.data.frame()
    }
    names(jg)<-"共记载物种数"
  }
  if(data=='bio'){
    bi<-tapply(m1$biomass,m1$plot,sum)%>%as.data.frame()
    bi$plot<-row.names(bi)
    m2<-merge(m1,bi,by='plot')
    m2$.<-round( m2$.,2)
    if(by=='qc'){
      jg<-tapply(m2$.,m2$群从,function(x)paste(min(x),"~",max(x)))%>%as.data.frame()
      jg$mean<-tapply(m2$.,m2$群从,mean)%>%round(.,2)
    }
    if(by=='qcz'){
      jg<-tapply(m2$.,m2$群从组,function(x)paste(min(x),"~",max(x)))%>%as.data.frame()
      jg$mean<-tapply(m2$.,m2$群从组,mean)%>%round(.,2)
    }
    if(by=='yf'){
      jg<-tapply(m2$.,m2$site,function(x)paste(min(x),"~",max(x)))%>%as.data.frame()
      jg$mean<-tapply(m2$.,m2$site,mean)%>%round(.,2)
    }
    names(jg)<-c('每平方米群落生物量','平均生物量')
  }
  
  jg
}



