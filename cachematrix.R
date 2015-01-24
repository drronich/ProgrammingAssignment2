# Makes a special list containing a function to
#  - set the value of the matrix (set)
#  - get the value of the matrix (get)
#  - set the value of the inverse matrix (setinverse)
#  - get the value of the inverse matrix (getinverse)
#
#  Args:
#    x - the given matrix
#
#  Returns:
#    A special list instance representing the given matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Makes a cacheable version of an inverse matrix containing within the given special list x.
#
#  Args:
#    x - the special list containing the given matrix
#
#  Returns:
#    A matrix that is the inverse of the matrix representing by 'x'
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}