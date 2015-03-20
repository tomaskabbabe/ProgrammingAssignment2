## This program consists of two different functions. One that creates a 
## special type of matrix-like object, which may have its inverse in the cache.
## The other function finds the inverse of a given matrix, but it first checks 
## whether the inverse is already in the cache avoiding an expensive computation


## In the function below we create our object, consisting of a list of functions allowing us to
## set the matrix, get the matrix, set the inverse and get the inverse, in that order. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) { 
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function below computes the inverse of a matrix, but checks first if the object
## already has its inverse in the cache.

cacheSolve <- function(x, ...) {
    
    #check if inverse exists in cache and returns it.
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    #compute inverse if not in cache and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
