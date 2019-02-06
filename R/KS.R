KS<-function(sp,by='family'){
  jg<-sp
  jg$FAMILY_CN<-paste(jg$FAMILY_CN,jg$FAMILY_APGIII)#将科名加入科名的拉丁


  if (by=='group') {
    g_k<-tapply(jg$FAMILY_APGIII,jg$GROUP,function(x)length(unique(x)))#每个group内科的数量
    g_s<-tapply(jg$GENUS,jg$GROUP,function(x)length(unique(x)))#每个group内属的数量
    g_z<-tapply(jg$SPECIES,jg$GROUP,length)#每个group内种的的数量
    ks_fx<-cbind(g_k,g_s,g_z)%>%as.data.frame()
    names(ks_fx)<-c("科","属","种")
  }

  if (by=='family') {
    k_s<-tapply(jg$GENUS,jg$FAMILY_CN,function(x)length(unique(x))) ###每个科内，属的数量情况
    k_z<-tapply(jg$FAMILY_APGIII,jg$FAMILY_CN,length) ####每个科内，种的数量情况
    k_zper<-k_z/sum(k_z)*100
    ks_fx<-cbind(k_s,k_z,k_zper)%>%as.data.frame()
    ks_fx<-data.frame(科名=tstrsplit(row.names(ks_fx)," ")[1],科=tstrsplit(row.names(ks_fx)," ")[2],
                        属数=ks_fx[,1],种数=ks_fx[,2],种所占比例=ks_fx[,3])
    names(ks_fx)<-c("科名","FAMILY_APG","属数","种数","种所占比例(%)")
    ks_fx<-arrange(ks_fx,-种数)
    ks_fx[,5]<-round(ks_fx[,5],2)
  }
  if(by=='genus'){
    s_z<-tapply(jg$SPECIES,paste(jg$GENUS_CN,jg$GENUS),length)%>%as.data.frame()
    s_z$per<-s_z/sum(s_z)*100
    ks_fx<-data.frame(属名=tstrsplit(row.names(s_z)," ")[1],
                        GENUS=tstrsplit(row.names(s_z)," ")[2],种数=s_z[,1],种所占百分比=s_z[,2])
    names(ks_fx)<-c("属名","GENUS","种数","种所占比例(%)")
    ks_fx<-arrange(ks_fx,-种数)
    ks_fx[,4]<-round(ks_fx[,4],2)
  }
  ks_fx
}


