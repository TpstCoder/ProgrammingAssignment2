## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix create a special matrix that is able to chache
## the inversion of itself
## cacheSolve returns the inversion of such special matrices created
## by makeCacheMatrix

## Write a short comment describing this function
## Create a matrix with its inversion cached, and provide methods to 
## access these data.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## Write a short comment describing this function
## First check if the input data have cached inversion.
## If yes, return that value. If not, calculate the inversion,
## cache the value, and return it.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setmean(i)
	i
}
