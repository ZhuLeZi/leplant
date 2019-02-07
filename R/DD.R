

DD<-function(x){
  a<-x
  du<-str_locate(a, "[°]")
  fen<-str_locate(a, "[′]")
  miao<-str_locate(a, "[″]")
  du<-du[,1]
  fen<-fen[,1]
  miao<-miao[,1]
  d<-str_sub(a, start = 0, end = du-1)
  f<-str_sub(a, start = du+1, end = fen-1)
  m<-str_sub(a, start = fen+1, end = miao-1)
  options(digits=10)
  d<-as.numeric(d)
  f<-as.numeric(f)
  m<-as.numeric(m)
  d1<-round(d,digits = 10)
  f1<-round(f,digits = 10)
  m1<-round(m,digits = 10)
  dx<-d1+f1/60+m1/3600

  dx
}
