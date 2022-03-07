# Caching the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        g <- NULL
        set <- function(y){
                x <<- y 
                g <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) g <<- inverse
        getInverse <- function() g
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function

# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
        g <- x$getInverse()
        if (!is.null(g)) {
                message("getting cached data")
                return(g)
        }
        data <- x$get()
        g <- solve(data, ...)
        x$setInverse(g)
        g
}
