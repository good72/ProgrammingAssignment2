
## Description:
##   Creates a matrix object that will cache the inverse matrix when 
## it is calculated.
##
## Usage :
## j <- matrix( c(1,2,3,4,5,-5,0,-1,-2),nrow=3)   # prep
## k <- makeCacheMatrix(j)                        # create it
## k$get()                                        # retrieve it
## k$getinv()                                     # returns NULL
## cacheSolve(k)                                  # returns Inverse
## k$getinv()                                     # now returns inverse
## cahceSolve(k)                                  # returns inverse, without calc
## cacheSolve(k) %*% k$get()                      # close to identity matrix
##
## k$set( j %*% j )                               # set matrix to new value
## k$getinv()                                     # returns NULL
## cacheSolve(k)                                  # recalc cached inv

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


## cacheSolve: returns inverse of the matrix, using a cached value if
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


