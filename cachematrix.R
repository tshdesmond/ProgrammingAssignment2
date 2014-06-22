
## makeCacheMatrix returns an object that encapsulates a matrix with the 
## ability to cache its inverse. The object returned is a list of functions.
## Note: assumed that the matrix passed in is always invertible.

makeCacheMatrix <- function(x = matrix())
{
    inversedX <- NULL
    
    set <- function(y)
    {
        x <<- y
        inversedX <<- NULL
    }
    
    get <- function()
    {
        x
    }
    
    setInverse <- function(inverse)
    {
        inversedX <<- inverse
    }
    
    getInverse <- function()
    {
        inversedX
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve returns the inverse of a matrix derived from makeCacheMatrix.
## It calculates the inverse if the inverse is not cached, and then caches the 
## result.

cacheSolve <- function(x, ...)
{
    if (is.null(x$getInverse()))
    {
        m <- x$get()
        x$setInverse(solve(m, ...))
        message("Calculating inverse...")
    }
    x$getInverse()
}
