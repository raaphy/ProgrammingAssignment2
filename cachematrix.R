# 'specialM <- makeCacheMatrix(m)' generates a special matrix which can cache its invers.
# to get the inverse call 'cacheSolve(specialM)' calculates the inverse if necessary and returns is.

## 'makeCacheMatrix(m)' creates a special matrix from matrix 'm'
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() {
		x
	}
	setinverse <- function(inv_para) {
		inv <<- inv_para
	}
	getinverse <- function() {
		inv
	}
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## 'cacheSolve(specialM)' returns the inverse of 'specialM'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (is.null(inv)) {
		inv <- solve(x$get(), ...)	
		x$setinverse(inv)
		return(inv)
	} else {
		inv	
	}
}
