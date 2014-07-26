## The following two functions create a special object that stores a Matrix and   
## caches its inverse.

## The function makeCacheMatrix returns a list of functions that perform
## the follwoing:

## - Set the value of the matrix.
## - Get the value of the matrix.
## - Set the value of the inverse.
## - Get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        set <- function(y) {x <<- y
                            inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## cachesolve function Compute the inverse of the matrix. If the inverse 
## is already stored,it returns the cached inverse matrix. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
