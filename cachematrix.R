## Caching the inverse of a matrix
## This R function file cache's potentially time-consuming computations.

## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) { ##set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x ##get the value of the matrix
        setinversematrix <- function(solve) m <<- solve ##set the value of the inverse matrix
        getinversematrix <- function() m ##get the value of the inverse matrix
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}
