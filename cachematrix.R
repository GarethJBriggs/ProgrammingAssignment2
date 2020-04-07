## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
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

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inverse <- x$getsolve()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ,...)
        ##Check to see if solve has any further arguments 
        x$setsolve(inverse)
        inverse
        ##Run test matricies
        ## Return a matrix that is the inverse of 'x'
}




