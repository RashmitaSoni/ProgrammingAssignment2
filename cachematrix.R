## Program to compute inverse of matrix using caching
## makesCacheMatrix function takes a matrix as argument
## return a vector of getters and setters of matrix


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## calculates the inverse of the matrix using solve()
## takes previous calculated inverse from cache
## takes list argument from above function

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data) %*% data
	x$setinverse(m)
	m
}
