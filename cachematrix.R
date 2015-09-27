## Three functions below to evaluate the inverse of the matrix, and reduce the machine time by reusing the results which have been already calculated
## makeCacheMatrix create a vector of 5 functions to write and read the input and the results

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                old <<- y
        }
        get <- function() x
        getold <- function() old
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv,
		 getold = getold)
}

## Matrix comparison function
matequal <- function(x, y) {
	is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
} 

## cacheSolve checks whether the result is already available for the same matrix, and report it from the memory if found; otherwise it calculates it and saves in the memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getinv()
        if(!is.null(m)) {
		    data <- x$get()
		    olddata  <- x$getold()
		    diff <- matequal(data,olddata)
		    if(diff) {
                message("getting cached data")
                return(m)
		    }
		    else {
		    message("different matrix")
		    return()
		    }
        }
        data <- x$get()
	  x$set(data)
        m <- solve(data, ...)
        x$setinv(m)
        m 
}
