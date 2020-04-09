## Put comments here that give an overall description of what your
## functions do

## makecacheMatrix takes a matrix as 'x' and creates a matrix object that
## comprises a list of functions that are getters and setters. These facilitate 
## the retreval and manifulation of matrix 'x' and its inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function (y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inverse <<- solve
        getsolve <- function() inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cachesSove checks whether the inverse of 'x', the matrix object returned by 
## makeCachmatrix has been caluclated. If so the inverse is retrieved from cache.
## If the inverse has not been calculated cachemean proceeds to calculate and
## return the inverse 

cacheSolve <- function(x, ...) {
        inverse <- x$getsolve()
        ## logical test to see if inverse of 'x' has been calculated previously, 
        ## i.e. is non-null
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        ## Claculatiion of the martix inverse
        inverse <- solve(data, ...)
        ## Setsolve functon in 'x' acquires the value of the inverse
        x$setsolve(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}




