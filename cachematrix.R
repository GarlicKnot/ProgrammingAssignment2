## Put comments here that give an overall description of what your
## functions do

## Inputs a square matrix into a cache function with list output
## Intended for inversion in cacheSolve

makeCacheMatrix <- function(x = matrix())  {
  m = NULL
  set    <- function(y) {
    x   <<- y
    m   <<- NULL
  }
  get    <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Takes a matrix converted to list through 'make CacheMatrix'
## Checks for cached data, and returns inverse matrix
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinv(m)
  m
}