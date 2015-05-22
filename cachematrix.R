## Put comments here that give an overall description of what your
## These two functions put the inverse of the matrix x into cache
## so when the inverse is needed after it is calcuated for the first
## time, the inverse can be obtained without calculation

## makeCacheMatrix convert a regular matarix x into
## a special "matrix" which can be used by the 
## cacheSolve function later. The output is acutally a list of four functions

makeCacheMatrix <- function(x = matrix()) {
      ii <- NULL 
 	set <- function(y) {
		x <<- y
		ii <<- NULL
 	}
	get <- function() x
	setinverse <- function(inverse) ii <<- inverse
	getinverse <- function() ii
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## cacheSolve calcualte the inverse of the "matrix"
## created by makeCacheMatrix. If the inverse of the
## matrix has been calculated, this function just obtain
## the cached result without calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	ii <- x$getinverse()
	if(!is.null(ii)) {
		message("getting cached data")
		return(ii)
	}
	data <- x$get()
	ii <- solve(data, ...)
	x$setinverse(ii)
	ii
}
