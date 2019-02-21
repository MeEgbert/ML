data=read.csv("iris.csv")

dist<-function(data){
  n<-length(data)
  m<-unique(lengths(data))
  c<-c()
  for(i in 1:n-1){
    r<-max(data[,1])-min(data[,1])
    c<-c(c,r)
  }
  M<-matrix(0,nrow=m,ncol=m)
  for(i in 1:m){
    for(j in i:m){
      if(data[i,5]==data[j,5]){tp=0}else{tp=1}
      r<-0
      for(p in 1:n-1){
        r<-r+abs(data[i,p]-data[j,p])/c[p]
      }
      M[i,j]<-(r+tp)/n
    }
  }
  return(M)
}