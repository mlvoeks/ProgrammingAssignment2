##These functions are designed to cache a matrix and 
## allow it to be retrieved and then to allow the inverse of the matrix to be cached
## and retrieved.

## This function caches matrix data and caches a NULL value to m... then allows the
## data to be retrieved from cache.

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## This function queries the cached value of an inverse of the matrix provided in 
## the makeCacheMatrix function and if none exists it creates and caches the inverse matrix
## and prints to the screen.  If the invers matrix already exists it sends a message
## to the screen saying "getting cached data" and then prints the inverse matrix

cacheSolve <- function(x, ...) {

    
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
