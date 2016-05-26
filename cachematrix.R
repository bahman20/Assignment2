## makeCacheMatrix is really a list of functions. cacheSolve function computes the 
## inverse of the special "matrix" returned by makeCacheMatrix. 

## Within the makeCacheMatrix function, "set" function sets the matrix to a new 
## matrix y, "get" returns the matrix, "setinv" sets the inverse of the matrix, and 
## "getinv" returns the inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- Null
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## In the cacheSolve function, if the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
