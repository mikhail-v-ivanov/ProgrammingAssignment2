# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than computing it repeatedly 
# (there are also alternatives to matrix inversion that we will not discuss here). 
# code assumes matrix supplied is always invertible.

# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv  <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) xinv <<- inverse
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

# cacheSolve: 
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve should retrieve the inverse from 
# the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xi <- x$getinv()
        if (is.null(xi)) {
                xi <- solve(x$get())
                x$setinv(xi)
        }
        else {
                message("Returning cached data.")
        }
        xi
}
