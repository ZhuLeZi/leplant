DOCC<-function(text,style){
  doc.path<-system.file("data", "nd.docx",
                        package = "leplant")

  doc<-read_docx(doc.path)

  i <- 1
  totalcol <- length(text)

  for(i in c(i:totalcol)){
    cs<-body_add_par(doc, text[i], style = style[i])
  }
  cs

}
