fcplot<-function(f1,cover=2,su=100){
  co<-cover
  su<-su
  fc<-subset(f1,cover>co)
fc<-arrange(fc,-cover)

  fc$x<-fc$cover/2
  fc$width=fc$cover
  fc$y<-(fc$hmin+fc$hmax)/2
  fc$height<-fc$hmax-fc$hmin

  pp<-ggplot(fc,aes(x,y,label=D,fill = D))  +
  geom_tile(aes(height=height,width=width,alpha=0.5))+
  geom_text(aes(x,y-(height/2)-1))+
    coord_cartesian(xlim = c(0,su))+
  theme_bw()+theme(legend.position='none')+
    labs(x="盖度 cover (%)",y="高度 height (cm)")
  pp
}


# geom_tile(aes(x=su/2,y=max(fc$hmax)/2,width=su,height=max(fc$hmax),alpha=0.2))+
#coord_cartesian(xlim = c(0,100))+
