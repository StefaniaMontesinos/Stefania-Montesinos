## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(x=matrix()){
  inv=NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<function() x
  setinv<-function(inverse) inv<<-inverse
  getinv<-function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}

cacheSolve<- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  Math <- x$get()
  inv <- solve(Math, ...)
  x$setinv(inv)
  inv
}

