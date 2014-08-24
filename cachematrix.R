## Matrix inverse calculation functions with the ability of caching the result
## for large matrices. 


## Function that can store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	# 'x' is the matrix
	# 'i' will cache the inverse matrix. We don't know the value at the beginning
        i <- NULL
        set <- function(y) {
		# The user want's to change the value of the matrix. We set the value, and remove the old cached inverse value in 'i'
                x <<- y
                i <<- NULL
        }
	# Gives back to the user the value of the matix
        get <- function() x 
	# Caches the inverse of the matrix
        setinverse <- function(inverse) i <<- inverse 
	# Returns the cached value of the inverse
        getinverse <- function() i 
	# Returns a list of supported methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function receives a makeCacheMatrix object and returns the inverse of the matrix.
## If it has been already calculated it will retreive the value from the cache thus saving computing time.

cacheSolve <- function(x, ...) {
	# Retrieve the inverse value. If it is not null return it directly
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
	# If executions continues at this point, 'i' value was null and we need to calculate its inverse. Cache it and return it.
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
}
