##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()) {
## Creates a list of functions that can cache the inverse of a matrix.

	m <- NULL
	set <- function(y) {
	x <<- y
	m <<- NULL
	}

	get <- function() x
	setInverse <- function(inverse) m <<-inverse
	getInverse <- function() m
	list(set = set, get = get,
	setInverse = setInverse,
	getInverse = getInverse)
 
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
## Computes the inverse of the matrix returned by makeCacheMatrix()

	m <- x$getInverse()
	if ( ! is.null(m)) {
	print("getting cached data")
	return(m)
	}

	m <- solve(x$get())
	x$setInverse(m)
	m

}
