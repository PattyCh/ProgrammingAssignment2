## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse.
## set: set the value of the matrix
## get: get the value of the matrix
## setinv: set the value of the inverse of the matrix
## getinv: get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      invx <- NULL
      set <- function (y){
          x <<- y
          invx <<- NULL
      }
      get <- function() x
      setinv <- function(inv) invx <<- inv
      getinv <- function() invx
      list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

    invx <- x$getinv()
    if(!is.null(invx)){
        message("Getting cached matrix")
        return(invx)
    }
    xmatrix <- x$get()
    invx <- solve(xmatrix)
    x$setinv(invx)
    invx
}
