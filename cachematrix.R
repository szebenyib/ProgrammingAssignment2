## Inverse matrix cacher. The functions below can be used to improve
## the speed of inversion by caching. The makeCacheMatrix creates a
## list of functions to get/set the matrix and get/set its inverse
## while cacheSolve uses this function to speed up the computation.

## Creates a list of functions to get/set the matrix and its inverse.
## Use: get, set, getinverse, setinverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(matrx) {
		x <<- matrx
		inverse <<- NULL
	}
	get <- function() {
		return(x)
	}
	setinverse <- function(inverted) {
		inverse <<- inverted
	}
	getinverse <- function() {
		return(inverse)
	}
	return(list(set = set,
		    get = get,
		    setinverse = setinverse,
		    getinverse = getinverse))
}

## Computes the inverse of a matrix using caching to speed up the
## computation. Returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        # invert only if no inverse exists
        if(is.null(inverse)) {
          originalmatrix <- x$get()
          inverted <- solve(originalmatrix)
          x$setinverse(inverted)
        } else {
          message("using cached data")
          return(inverse)
        }        
}