# This program is to compute the inverse of matrix and avoid to compute repeatedly
# The output will be cached and ready for reuse.

# The function implementes the computation of inverse matrix and caching

makeCacheMatrix <- function(x=matrix()) {
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <-function()x
  setinverse <-function(inverse) m<<-inverse
  getinverse <-function() m
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

# The function checks if there is cached results, otherwise it will call the function above to compute and cache.

cacheSolve <- function(x,...) {
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cashed data")
    return(m)
  }
  data <-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}
