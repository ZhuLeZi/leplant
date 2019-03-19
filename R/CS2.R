CS2<-function(f,cs){
f$p<-paste(f$site,f$D)
f$p<-str_replace_all(f$p,"层片","")

cs$p<-paste(cs$site,cs$D)

jg2<-left_join(cs,f[,3:7],by="p")
jg2
}