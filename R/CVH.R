

web2<-read_html("http://www.cvh.ac.cn/search/%E5%A4%A7%E9%92%88%E8%8C%85?page=1&searchtype=1&n=2")
len<-web2%>%html_nodes(., "div.scott a")%>%html_text()%>%as.numeric()%>%max(.,na.rm = T)



CVH<-function(sp){
sp<-URLencode(sp)
web2<-paste("http://www.cvh.ac.cn/search/",sp,"?page=1&searchtype=1&n=2",sep = "")%>%read_html()
len<-web2%>%html_nodes(., "div.scott a")%>%html_text()%>%as.numeric()%>%max(.,na.rm = T)


i<-1
jg<-list()
zl<-data.frame()
for(i in i:len){
  jg[[i]]<-paste("http://www.cvh.ac.cn/search/",sp,"?page=",i,
                 "&searchtype=1&n=2",sep = "")%>%read_html()%>%LB()
}

zl<-do.call(rbind,jg)
zl

}

LB<-function(web2){
tab<-web2%>%html_table(header = T,fill = TRUE)
ta<-tab[4]%>%as.data.frame()
ta
}

###########
UR<-function(bh){
  
  
  ul<-"http://www.cvh.ac.cn/spm/"
  
  b<-str_replace_all(bh," ","/")
  jg<-paste(ul,b,sep="")
  jg
}

SPIN<-function(bh){
  i<-1
  len<-length(bh)
  jg<-list()
  for(i in i:len){
    jg[[i]]<-SPINF(bh[i])
  }
 zl<-do.call(rbind,jg)%>%as.data.frame()
 zl
}

SPINF<-function(spurl){
  sp<-UR(spurl)
  webin<-read_html(sp)
  
  js<-vector(length = 3)
  
  
  js[1]<-webin%>%html_nodes("div#o_spcollter")%>%html_text()
  js[2]<-webin%>%html_nodes("div#o_spcoldate")%>%html_text()
  js[3]<-webin%>%html_nodes("div#o_spplace")%>%html_text()
  
  js
}