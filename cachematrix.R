## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mx <- NULL
    set <- function(y) {
        x <<- y
        mx <<- NULL
    }
    get <- function() x
    setsolve <- function(z) mx <<- z
#    getmean <- function() m
        getsolve <- function() {
            if (is.null(mx)) {
           # Calculate the mean, then cache it
           print("Creating inv for the first time")
           mx <<-  x 
       } else {
           print("Using pre-calculated inv")
       }
       mx
    }
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mx <- x$getmean()
    if(!is.null(mx)) {
        message("getting cached data")
        return(mx)
    }
    data <- x$get()
    mx <- x
    x$setsolve(mx)
    mx
}
