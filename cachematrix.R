## These two functions will inverse a special matrix and cache it.

## This function will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){  
    x <<- y
    i <<- null
  }
  get <- function () x
  setinverse <- function (inverse) i <<- inverse
  getinverse <- function () i
  # uses the list() function to store the 4 function in makeCacheMatrix.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## function computes the inverse of the function created by makeCacheMatrix.
#the input (x) of cachesolve is the object where makeCacheMatrix is stored
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  # verifies that i isn't empty
  if(!is.null(i)){
     message("getting cached matrix")
     return (i)
  }
  #gets the matrix stored with makeCacheMatrix i
  data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
        ## Returns a matrix that is the inverse of 'x' using the solve function
}
