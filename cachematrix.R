## First function creates a matrix that caches the inverse 
## second function solves the inverse of makeCacheMatrix but checks the cache first

## makeCacheMatrix creates a "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<-function(y){
    x <<-y
    i <<- NULL
  }
  get<- function()x
  setinverse<- function() i <<- inverse
  getinverse<- function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## cacheSolve computes the inverse of makeCacheMatrix. 
## if the inverse has already been calculated it will retrieve it from the cache.

cacheSolve <- function(x, ...) {
  i<- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<- x$get
  i<- inverse(data, ...)
  x$setinverse(i)
  i
}


M<- matrix(c(1,2,3,4),2,2)
M
iM<-makeCacheMatrix(M)
cacheSolve(iM)
iM


