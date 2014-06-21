## These functions provides the ability to store a matrix and then cache it's inverse
## for later use to conserve computing resources.

## Creates a vector consisting of 4 fuctions - (1) Set the value of the matrix (2) Get the
## value of the matrix (3) Set the inverse (4) Get the inverse

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


## Calculates the inverse of the matrix, but checks first to see if the inverse has already been
## calculated.  If so, it recalls the previous calcuation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        ## If the inverse is already there, grab it...
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If not, calc it...
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
