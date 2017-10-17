## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function is used to solve the inverse of a square matrix which is solvable. 

makeCacheMatrix<-function(x=matrix()){ 
  inv=NULL
  set<-function(y){ ## with this function you can set the value of your matrix 
    x<<-y
    inv<<-NULL
  }
  get<function() x # in this part you get the value of the matrix so it can be used later 
  setinv<-function(inverse) inv<<-inverse # here you should set the value of the inverse 
  getinv<-function() inv # you get the value of the inverse you set before.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}

cacheSolve<- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data") # if the inverse was already calculated so it returns the value.
    return(inv) # 
  }
  Math <- x$get()
  inv <- solve(Math, ...)
  x$setinv(inv) # with this function it calculates the inverse if it was not already. 
  inv
}


