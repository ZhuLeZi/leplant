pot<-function(x){
  spp<-x
  p<-ggplot(data=spp,mapping=aes(x=Var1,y=per,group=factor(1)))
  p<-p+geom_bar(stat="identity",width = 0.5,fill = "grey")
  p<-p+geom_text(aes(label = paste(Freq,"种"), vjust = -2.5, hjust = 0.5), show.legend = TRUE)
  p<-p+geom_text(aes(label = paste(per,"%"), vjust = -0.8, hjust = 0.5), show.legend = TRUE)
  p<-p+theme_classic()
  p<-p+theme(axis.title.y = element_text(face = "bold"),
             axis.title.x = element_text(face = "bold"),
             axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1,colour = 'black'))
  p<-p+scale_y_continuous(expand = c(0,0),limits = c(0,max(spp$per)*1.3),breaks = seq(0,100,5) )
  p<-p+labs(y="百分比 Percentage (%)")
  p
}

