## These functions create an inverted matrix and cache the
## solution so that it can be retrieved without recomputing


## The first function creates the original matrix and a list
## of functions that will be used in conjunction with the 
## second function to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    invertMatrix <- function(answer) m <<- answer
    getInvert <- function() m
    list(set = set, get = get,
         invertMatrix = invertMatrix,
         getInvert = getInvert)
}



## This function checks to see if the matrix has already been
## inverted and returns that solution if it has, otherwise
## this function inverts the matrix and stores the solution
## in the first function above

cacheSolve <- function(x, ...) {
    m <- x$getInvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$invertMatrix(m)
    m
}