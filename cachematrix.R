##These two functions will cache the results of
##taking the inverse of a matrix.

##makeCacheMatrix creates a "matrix" object with
##several functions that will set and get
##the values of the inverse of a matrix.

makeCacheMatrix <- function(x=matrix()) {
  m<-NULL
  set<- function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve<- function(solve) m<<- solve
  getsolve<-function() m
  list(set=set,get=get,
       setsolve=setsolve,
       getsolve=getsolve)
  
}

##cacheSolve uses makeCacheMatrix as an input
##to solve (take the inverse of) a matrix or get the inverse
##from the cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

##returns a matrix that is the inverse of "x"