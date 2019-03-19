#层片划分函数，可根据需求自定义
CPHF<-function(sp){
  sp<-sp
  s<-sp$GENUS
  sh<-sp$生活型

  dis<-c("Carex" ="苔草层片",                                #苔草
         "Allium"="葱类植物层片",
         "Leymus"="根茎型禾草层片",                   #羊草
         "Calamagrostis"="根茎型禾草层片",          #拂子茅
         "Phragmites"="根茎型禾草层片",               #芦苇
         "Pennisetum"="根茎型禾草层片",              #白草
         "Psammochloa"="根茎型禾草层片",            #沙鞭

         "Cleistogenes"="丛生小禾草层片",               #隐子草
         "Agropyron"="丛生小禾草层片",                 #冰草
         "Poa"="丛生小禾草层片",                            #早熟禾
         "Ptilagrostis"="丛生小禾草层片",                     #细柄茅
         "Bothriochloa"="丛生小禾草层片",               #白羊草
         "Koeleria"="丛生小禾草层片",                    #洽草
         "Puccinellia"="丛生小禾草层片",              #碱茅
         "Stipa"="丛生禾草层片",                                  #针茅
         "Achnatherum"="丛生禾草层片",             #芨芨草
         "Elymus"="丛生禾草层片",                         #披碱草
         "Roegneria"="丛生禾草层片")                          #鹅观草

  dih<-c("乔木"="乔木层片",
         "灌木"="灌木层片",
         "小灌木"="灌木层片",
         "半灌木"="半灌木层片",
         "小半灌木"="半灌木层片",
         "一年生"="一二年生草本层片",
         "一年生草本"="一二年生草本层片",
         "一、二年生草本"="一二年生草本层片",
         "一、二年生"="一二年生草本层片",
         "二年生草本"="一二年生草本层片"
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
