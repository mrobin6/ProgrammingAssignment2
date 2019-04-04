## Caching the output of complex operations can be more
## efficient with computing power than carrying out the operation
##repeatedly. These functions work together to create a "matrix"
##object and then calculate and cache the inverse of that marix

## This creates a list that works as a matrix with the 
## following function and can store its own inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function checks if the inverse has already been stored, 
## and if not, calculates and stores the inverse of the matrix

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
