#'Euclidean algorithm
#'
#'find the greatest common divisor of two integers
#'
#'@param a a numeric scalar or integer
#'@param b a numeric scalar or integer
#'@return a integer number
#'@description find the greatest common divisor of two integers
#'@references \url{https:\\en.wikipedia.org/wiki/Euclidean_algorithm.}

euclidean<-function(a,b){
  stopifnot(is.numeric(c(a,b))==TRUE || is.integer(c(a,b))==TRUE)
  if(a<b){
    aux<-a
    a<-b
    b<-aux
  }
  while(b!=0){
    q=a%/%b
    r=a%%b
    a<-b
    b<-r
  }
  return(a)
}