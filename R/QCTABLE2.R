QCHF2<-function(x){
  qc<-x
  qc_t<-qcmm(qc)
  ####删除组内重复值
  cs<-tapply(qc_t$site, qc_t$群从,function(x)paste(x,collapse ="、"))%>%as.data.frame()
  cs1<-data.frame(样地号=cs[,1],群从=row.names(cs))
  jg<-merge(qc_t,cs1,by="群从")

  df<-jg[!duplicated(jg$群从),]%>%qcpx()

  df
}
#和新的QCHF结果对应
###x=t1
QCTABLE2<-function (x, out = "table")
{
  css <- QCHF2(x)
  css$群从组序号 <- LM(css$群从组序号)
  if (out == "mm") {
    qc <- paste(paste(css$群从组序号, "-", css$群从序号,
                      sep = ""), css$群从, css$Ass., paste("(", css$样地号,
                                                         ")", sep = ""))
  }
  if (out == "mm2") {
    qc <-css$Ass.%>%as.data.frame()
    HS<-function(a){
      aa<-str_replace_all(a,"-","，")%>%str_replace_all(.,"Ass.","Ass（")
      a2<-paste(aa,"）",sep="")
      a2
    }
    qc2<-apply(qc,2,HS)
    qc<-data.frame(CN=css$群从,Ass=qc2)
  }
  if (out == "table") {
    qc <- data.frame(群从组 = paste(css$群从组序号, ".",
                                 css$群从组, sep = ""), 群从 = paste(css$群从组序号,
                                                                "-", css$群从序号, css$群从, sep = ""))
  }
  if (out == "table2") {
    qc <- css
  }
  qc
}


