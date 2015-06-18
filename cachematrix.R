## These two functions will inverse a special matrix and cache it.

## This function will create a special "matrix" object that can cache its its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- null
  }
  get <- function () x
  setinverse <- function (inverse) i <<- inverse
  getinverse <- function () i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function computes the inverse of the function created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
     message("getting cached matrix")
     return (i)
  }
  data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
