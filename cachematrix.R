## Put comments here that give an overall description of what your
## functions do

## Description:
##   Creates a matrix object that will cache the inverse matrix when 
## it is calculated.

makeCacheMatrix <- function(x = matrix()) {
    
    mat <- x
    
    # stores cached inverse matrix
    cachedInv <- NULL
    
    # basic set fn - also invalidates current cached inverse
    set <- function(y) {
      mat <<- y
      cachedInv <<- NULL
    }
    
    # basic get fn
    get <- function() mat
    
    setinv <- function(inv) cachedInv <<- inv
    
    getinv <- function() cachedInv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve: returns inv of the matrix, using a cached value if
## there is one, otherwise calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   cachedInv <- x$getinv()
   if (is.null(cachedInv)) {
     cachedInv <- solve(x$get(), ...)
     x$setinv(cachedInv)
   }
   cachedInv
}


