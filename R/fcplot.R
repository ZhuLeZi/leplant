fcplot<-function(fc,cover=0.2){
  fc<-subset(fc,cover>0.2)
  fc$x<-fc$cover/2
  fc$width=fc$cover
  fc$y<-(fc$hmin+fc$hmax)/2
  fc$height<-fc$hmax-fc$hmin
  
  pp<-ggplot(fc,aes(x,y,label=D,fill = D))  + 
  geom_tile(aes(height=height,width=width,alpha=0.5))+
  geom_text(aes(x,y-(height/2)))+
  theme_classic()+theme(legend.position='none')
  pp
}
