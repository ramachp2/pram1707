makeCacheMatrix<-function(a=matrix()){
  inva<-NULL
  setInpMatrix<-function(b){
    a<<-b
    inva<<-NULL
  }
  getInpMatrix<-function() a
  matrix <-function(c) inva<<-a
  getInva<-function() inva
  list(setInpMatrix=setInpMatrix, getInpMatrix=getInpMatrix, matrix=matrix, getInva=getInva)
)

cacheSolve<-function(k, ...){
  inva<k$getInva()
  if (!null(inva){
    message("getting cached data")
    return(inva)
  }
  InpMat<-k.getInpMatrix()
  inva<-solve(InpMat, ...)
  k.matrix(inva)
  inva
}