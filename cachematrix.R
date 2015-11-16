## Put comments here that give an overall description of what your
## functions do

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
##  makeVector <- function(x = numeric()) {
##    m <- NULL
##    set <- function(y) {
##      x <<- y
##      m <<- NULL
##    }
##    get <- function() x
##    setmean <- function(mean) m <<- mean
##    getmean <- function() m
##    list(set = set, get = get,
##         setmean = setmean,
##         getmean = getmean)
##  }
      invx <- NULL
      set <- function (y){
        x <<- y
        invx <<- NULL
      }
      get <- function() x
      setinv <- function(inv) invx <<- inv
      getinv <- function() invx
      matrix(list(set, get, setinv,getinv),2,2,dimnames=list(c("set","get"),c("orig","inv")))
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
##  cachemean <- function(x, ...) {
##    m <- x$getmean()
##    if(!is.null(m)) {
##      message("getting cached data")
##      return(m)
##    }
##    data <- x$get()
##    m <- mean(data, ...)
##    x$setmean(m)
##    m
##  }
    invx <- x["get","inv"][[1]]()
    if(!is.null(invx)){
      message("Getting cached matrix")
      return(invx)
    }
    xmatrix <- x["get","orig"][[1]]()
    invx <- solve(xmatrix)
    x["set","inv"][[1]](invx)
    invx
}
