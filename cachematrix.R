## Put comments here that give an overall description of what your
## functions do

## creates special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <-function(y){
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i<<-inverse
     getinverse <- function() i
     list(set=set, get=get,
          setinverse=setinverse,
          getinverse=getinverse)
}

## compute inverse of special matrix from above function. can also retrieve inverse from cache if it exists

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
          measure("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data,...)
     x$setinverse(i)
     i
}
