


SAR<-function(x){
  herb<-x
  com<-herb[,2:3]%>%table()
  p_e<-specaccum(com)

  da<-data.frame(sites=p_e$sites,richness=p_e$richness,sd=p_e$sd)

  ggplot(da, aes(x=sites, y=richness)) +
    geom_errorbar(aes(ymin=richness-sd, ymax=richness+sd), width=.1)+geom_point(size=3, shape=21, fill="white")+
    scale_y_continuous(breaks=0:20*10)+
    theme_bw()
}



