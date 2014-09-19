## The functions makeCascheMatrix and cascheSolve will compute the inverse of a matrix and store the value in a cache to be used for further calculations without needing to solve for the inverse.

## makeCacheMatrix creates a list containing functions to store a matrix value, call the value of the matrix, set the inverse value of the matrix, and return the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
 		m<- NULL
        set <- function(y){
               	x<<- y
               	m<<- NULL
        }
        get <- function() x
        setinverse <- function(inv) m <<- inv
        getinverse <- function() m
        list(set = set, get = get, setinverse= setinverse, getinverse = getinverse)
}


## checks to see if the inverse of the matrix is already stored. If it is, the function returns the stored inverse value. If it is not stored, the function will solve the matrix and store the value of the inverse.

cacheSolve <- function(x, ...) {
 		m<- x$getinverse()
        if(!is.null(m)) {
                message("getting cashed inverse")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setinverse(m)
        m
}
