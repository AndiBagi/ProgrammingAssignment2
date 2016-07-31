## This pair of functions will cache the inverse of a matrix

## The first function creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function will calculate the inverse of a matrix 
## It first checks if the inverse 's' has already been calculated
## If yes, it gets the inverse from the cache, skips the computation and 
## shows the message "getting cached data". 
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
        if(!is.null(s)) {
            message("getting cached data")
        return(s)
        }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
