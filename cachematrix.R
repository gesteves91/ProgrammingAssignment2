## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates and returns a list of functions
makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        
        # create the matrix in the working environment
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }

        # get the matrix value
        get <- function() x
        setMatrix <- function(inverse) cache <<- inverse
        getInverse <- function() cache

        # return the functions
        list(set = set, get = get,
        setMatrix = setMatrix,
        getInverse = getInverse)
}

## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix
cacheSolve <- function(x, ...) {
        cache <- x$getInverse()

        # return inverted matrix from cache if it exists
        if (!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }

        # create the matrix
        matrix <- x$get()

        tryCatch( {
                cache <- solve(matrix, ...)
        },
        error = function(e) {
                message("Error:")
                message(e)
                return(NA)
        },
        warning = function(e) {
                message("Warning:")
                message(e)
                return(NA)
        },
        finally = {
                x$setMatrix(cache)
        } )

        return (cache)
}
