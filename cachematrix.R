## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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

init <- function(mat){
    x <<- makeCacheMatrix()
    x$set(mat)
    if (!identical(x$get, mat))
        message("identical")
}