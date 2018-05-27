## Assignment 2
## Created by L. Campbell

## Objective:  Below are two functions which work together to create
## an object that stores a matrix and caches the inverse of that matrix.

## The first function builds a list of four functions pertaining to the
## objective stated above.
##	1. Sets the value of the matrix (x) in the cache
##	2. Gets the value of the matrix (x) from the cache
##	3. Sets the value of the inverse (m) in the cache
##	4. Gets the value of the inverse (m) from the cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function calculates the inverse of a matrix.
## Before doing so, it checks to see if the inverse is already available.
## If it is, it 'gets' it from the cache - avoiding the computation.
## If not, it computes the inverse and 'sets' it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
