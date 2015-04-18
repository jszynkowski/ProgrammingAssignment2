##This set of functions takes and finds the inverse of a sqaure invertable
##matrix and caches it for further use

##Provide the functions to store and retreive the cached matrix.  

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        k <<- y
        inv <<- NULL
    }
    get <- function() k
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Check to see if there is an inverse matrix stored in the cache. If so return
##it, if not find the inverse with solve() and store it in the cache for later 
##use. 

cacheSolve <- function(j, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("retriving inverse from cache...")
        return(inv)
    }
    data = x$get()
    inv = solve(data, ...)
    x$setinverse(inv)
    inv
}