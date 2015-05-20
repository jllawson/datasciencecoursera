## The following functions cache and calculate the inverse of a matrix.
##
## Usage Example:
## source('cachematrix.R')
## > m <- makeCacheMatrix()
## > a$set(matrix(1:4,2,2))
## > cacheSolve(a)
##      [,1] [,2]
## [1,]  -2   1.5
## [2,]   1  -0.5


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## If the inverse has already been calculated, the result is retrieved from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
}
