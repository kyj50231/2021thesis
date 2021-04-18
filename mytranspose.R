mytranspose = function(x) {
if(is.matrix(x)){
 y <- matrix(1, nrow=ncol(x), ncol = nrow(x))
 if(nrow(y)<1){
  y<-y
 }else{
   for(i in 1:nrow(x)) {
    for(j in 1:ncol(x)) {
     y[j,i] <- x[i,j]
    }
   }
 }
}else if(is.vector(x)){
 y <- matrix(1, nrow=length(x), ncol = 1)
  for(i in 1:length(x)) {
   y[i,1] <- x[i]
  }
}else if(is.data.frame(x)){
 y <- t(x)
}
else{
 return(FALSE)
}
 return(y)
}
