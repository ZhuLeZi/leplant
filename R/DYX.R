DYX<-function(iv){
  hdata<-as.data.frame(iv)
  hdata<-t(hdata)
  hdata<-as.data.frame(hdata)

  i <- 1
  totalcol <- ncol(hdata)
  h <- vector(length = totalcol)
  D <- vector(length = totalcol)
  J <- vector(length = totalcol)
  N <- vector(length = totalcol)
  #
  for(i in i:totalcol){
    a <- names(hdata)[i]
    tempdata <- hdata[[a]]
    tempdata <- na.omit(tempdata)
    h[i] <- -1*sum(tempdata*log(tempdata))
    D[i] <- 1-sum(tempdata^2)
    J[i] <- h[i]/log(length(tempdata))
    N[i] <- length(tempdata)
  }
  site<-as.numeric(rownames(iv))
  haa <- cbind(h,D,J,N,site)
  haa<-as.data.frame(haa)
  names(haa) <- c("Shannon Wiener","Simpson","pielou","S","site")
  haa<-arrange(haa,site)
  haa
}
