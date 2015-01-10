## make Matrix that is capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## i = inverse, x = matrix
    i <- NULL
    set <- function(m) {
        x <<- m
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        data <- x$get()
        inverse <- solve(x)
        x$setInverse(inverse)
        inverse
}
