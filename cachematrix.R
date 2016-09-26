## The following two functions save computing time by "caching" the inverse
## value of a given matrix, thereby saving it into an environment that is
## different from the current environment.  The functions check to see if the
## inverse has already been calculated and if so, retrieve that value rather
## than re-calculating it.  If the inverse value does not exist in the cache,
## it is calculated and saved as such.

## The makeCacheMatrix function creates a list of functions that caches the
## value of a given matrix into cache, gets that value, caches the value of the
## matrix inverse, and gets that inverse.  This cached value will be drawn from
## when using the next function below.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function first checks to see if the inverse of the cached
## matrix is already calculated.  If so, it returns that value from cache.  If
## not, it calculates the inverse anew and sets that value in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}