
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # to set the value to the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatinverse <- function(inverse) m <<- inverse
    getmatinverse <- function() m
    #creates list of functions
    list(set = set, get = get,
         setmatinverse = setmatinverse,
         getmatinverse = getmatinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    #inverse the matrix 'x' and return it
        m <- x$getmatinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #do the matrix inverse
        x$setmatinverse(m) 
        m #return the result
    }


