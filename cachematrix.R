##############################################################################
# Write the following functions:
#   makeCacheMatrix: This function creates a special "matrix" object that can 
#   cache its inverse.
#   cacheSolve: This function computes the inverse of the special "matrix" 
#   returned by makeCacheMatrix above. If the inverse has already been 
#   calculated (and the matrix has not changed), then the cachesolve should 
#   retrieve the inverse from the cache.
#   Computing the inverse of a square matrix can be done with the solve function 
#   in R. For example, if X is a square invertible matrix, then solve(X) returns 
#   its inverse.
#
# For this assignment, assume that the matrix supplied is always invertible.
##############################################################################
# makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
##############################################################################
makeCacheMatrix <- function(x = matrix()) 
{
    # initialize cache 
    cachedMatrix <- NULL
    
    # store matrix
    setMatrix <- function(matrixValue) 
    {
        x <<- matrixValue
        
        # initialize cache
        cachedMatrix <<- NULL
    }
    
    # return cached matrix
    get <- function() x
    setinverse <- function(inverse) cachedMatrix <<- inverse
    getinverse <- function() cachedMatrix
    list(setMatrix=setMatrix, get=get, setinverse=setinverse, getinverse=getinverse)
}


##############################################################################
# cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
##############################################################################
cacheSolve <- function(x, ...) 
{
    # retrieve cached value
    inv <- x$getinverse()
    
    # return cached value if exists
    if(!is.null(inv)) 
    {
        message("getting cached data.")
        
        # return inverse matrix
        return(inv)
    }
    else
    {
        message("computing inverse matrix.")
        
        # compute inverse matrix and store in cache
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        
        # return inverse matrix
        return(inv)
    }
}
