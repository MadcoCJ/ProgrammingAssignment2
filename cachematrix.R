## The following functions will create an object that stores a matrix and cache
## its inverse

## This function will:
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse of the matrix
##      get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}


## This function will return a matrix that is the inverse of 'x'
##      First it will check to see if the inverse of 'x' has already been 
##      calculated. 
##      If it has, the inverse will be retrieved from the cache
##      and printed.
##      If the inverse has not already been calculated, it will be calculated
##      then stored in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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