#层片划分函数，可根据需求自定义
CPHF<-function(sp){
  sp<-sp
  s<-sp$GENUS
  sh<-sp$生活型
  dis<-c("Carex" ="苔草层片",
         "Allium"="葱类植物层片",
         "Leymus"="根茎型禾草层片",
         "Calamagrostis"="根茎型禾草层片",
         "Phragmites"="根茎型禾草层片",
         "Pennisetum"="根茎型禾草层片",
         "Psammochloa"="根茎型禾草层片",

         "Cleistogenes"="丛生小禾草层片",
         "Agropyron"="丛生小禾草层片",
         "Poa"="丛生小禾草层片",
         "Ptilagrostis"="丛生小禾草层片",
         "Bothriochloa"="丛生小禾草层片",
         "Koeleria"="丛生小禾草层片",

         "Stipa"="丛生禾草层片",
         "Achnatherum"="丛生禾草层片",
         "Elymus"="丛生禾草层片",
         "Roegneria"="丛生禾草层片")

  dih<-c("乔木"="乔木层片",
         "灌木"="灌木层片",
         "小灌木"="灌木层片",
         "半灌木"="半灌木层片",
         "小半灌木"="半灌木层片",
         "一年生"="一二年生草本层片",
         "一年生草本"="一二年生草本层片",
         "一、二年生草本"="一二年生草本层片",
         "一、二年生"="一二年生草本层片"
  )

  i<-1
  total<-length(sp$TAXA_NAME)
  D <- vector(length = total)
  for(i in i:total){
    s_jg<-dis[s]
    sh_jg<-dih[sh]

    D[which(is.na(D))]<-"杂类草层片"
  }

  jg<-data.frame(s_jg,sh_jg)
  jg_p<-paste(jg$s_jg,jg$sh_jg,sep="")%>%str_replace_all(.,"NANA","杂类草层片")%>%str_replace_all(.,"NA","")



  sp$D<-jg_p
  sp
}
