## The function makeCacheMatrix initializes a special type of matrix
# that caches its inverse. Getter & Setter functions are available wirhin
# to get/set the value of the matrix, as well as get/set the value of 
# the inverse

makeCacheMatrix <- function(mat = matrix()) {
  matrixInverse <- NULL
  
  set <- function(x) {
      mat <<- x
      matrixInverse <<- NULL
  }
  get <- function() mat
  
  setinverse <- function(inv) matrixInverse <<- inv
  getinverse <- function() matrixInverse
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function calculates the inverse of the matrix and caches it
# If the inverse is already calculated and cached, then the value is returned
# from the cache, rather than recalculating the matrix inverse
cacheSolve <- function(mat, ...) {
  matrixInverse <- mat$getinverse()
  if(!is.null(matrixInverse)) {
    message("Retrieving from cache...")
    return(matrixInverse)
  }
  mtx <- mat$get()
  matrixInverse <- solve(mtx, ...)
  mat$setinverse(matrixInverse)
}
