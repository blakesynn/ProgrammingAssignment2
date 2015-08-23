## The following functions allow you to cache the inverse of a matrix so
## that you do not need to calculate the inverse every time, saving you
## time.

## The makeCacheMatrix function is essentially storage for a list of 
## 4 functions (set, get, setsolve, getsolve) which you can use individually
## by subsetting them.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve uses an if/then statement to first search to see if the inverse
## has been cached, and returns it with a message if it has.  If it has not, 
## it uses the solve() function to get the inverse, then cache it in setsolve()

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}
