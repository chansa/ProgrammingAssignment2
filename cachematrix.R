## Pair of functions that cache the inverse of a mtrix. Caching to reduce the
## cost of computation.


## function creates a special matrix object that can cache its inverse.
## this matrix creates a list of functions to get,set value of matrix And
## to get& set the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {

	inv<- NULL
	set <- function(y) {
		x<<- y
		inv<<- NULL
	}
	
	get<- function() x
	setinv<- function(inverse) inv<<- inverse
	getinv<- function() inv

	list(set=set,get=get, setinv = setinv, getinv = getinv)

}


## Following function computes the inverse of special matrix created with makeCacheMatrix()
## This also checks first whether inverse had already been computed or not.
## else it computes it using solve()

cacheSolve <- function(x, ...) {   ## Return a matrix that is the inverse of 'x'

	inv <- x$getinv()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)	
	} 			

	data<- x$get()
	inv<- solve(data,...)
	x$setinv(inv)	
	inv       
}
