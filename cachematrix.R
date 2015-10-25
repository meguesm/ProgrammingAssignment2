## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    set <- function(y) {
        x <<- y
        mx <<- NULL
    }
    get <- function() x
    setinv <- function(z) mx <<- z
    getinv <- function() {
        if(!is.null(mx)) {
            message("getinv: getting cached data")
        }
        mx
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mx <- x$getinv()
    if(!is.null(mx)) {
        message("cacheSolve: getting cached data")
        return(mx)
    }
    data <- x$get()
    mx <- solve(data)
    x$setinv(mx)
    mx
}
