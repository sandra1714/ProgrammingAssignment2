## makeCacheMatrix creates a special object in the form of a list containing the following 
## elements: set the values in a matrix, get the values in the matrix, set the values in the
## inverse matrix, get the values in the inverse matrix.
## cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then  
## cachesolve returns the inverse from the cache instead.

## This function creates the object with the matrix, its inverse and functions as described above.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) { # set the values in a matrix
        x <<- y
        I <<- NULL
    }
    get <- function() x # get the values in a matrix
    setSolve <- function(solve) I <<- solve # calculate the inverse of a matrix
    getSolve <- function() I # get the inverse of a matrix
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function returns the cached inverse of an input matrix if already available, 
## otherwise it calculates one.

cacheSolve <- function(x, ...) {
    I <- x$getSolve() # get the inverse of the matrix
    if(!is.null(I)) { # if the returned value is not null, return the cached inverse
        message("getting cached data")
        return(I)
    }
    data <- x$get() # if the matrix inverse is not computed, get the values in the matrix
    I <- solve(data, ...) # get the inverse of the matrix
    x$setSolve(I) # set the inverse of the matrix in the object
    I # return the inverse of the matrix
}
