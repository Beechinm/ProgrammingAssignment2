## These two functions create a special object that stores a matrix and caches its inverse.

## The first function "makeCacheMatrix" creates a special "matrix" that is used to store a list of functions that
## set the matrix, get the matrix, set the matrix's inverse, and get the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## the "cacheSolve" function is used to find the inverse of the special "matrix" created with the above function.
## it first checks to see if the inverse already exists by using the getinverse function, otherwise
## it returns "getting cached data". if not it calculates the inverse of the matrix using the solve 
## function and stores it in the cache using the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <-x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
