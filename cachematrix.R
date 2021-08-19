# Put comments here that give an overall description of what your
## functions do
#Week 3 Programming Assignment
#Created by Sayantani Manna
## The mackCacheMatrix creates the "matrix" that can cache
## its inverse.


makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse){inv <<- inverse}
  getInverse <- function(){inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The cacheSolve computes the inverse of the "matrix"
## returcned by the makeCacheMatrix function above.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

