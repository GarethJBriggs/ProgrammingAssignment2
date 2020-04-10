##JHU R Programming Assignment #2
##Gareth James Briggs
##March 2020

## These functions enable the calculation of the inverse of a matrix and the inverse's caching. 
## The inverse need not be calculated again following the input of the same matrix, it can be  
## retrieved from cache.

## makeCacheMatrix function creates a matrix object 'x', as well as getters and setters to 
## cache the inverse and update and retrieve 'x, and also initialises the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        ##set function enables the resetting of both the inverse to null and of 'x' to the value
        ## of 'y', in the parent environment via the use of the deep assighnment operator
        set <- function (y) {
                x <<- y
                inverse <<- NULL
        }
        ## get function acquires value of 'x' from the parent environment
        get <- function() x
        ## setsolve function sets the value of the inverse within the parent environment  
        ## via thedeep assighnment operator, from the value of solve 
        setsolve <- function(solve) inverse <<- solve
        ## getsolve function acquires the value of the inverse from the parent evironment
        getsolve <- function() inverse
        ## functions put into a list as named elements so the getters and setters can 
        ## be accessed via subsetting using $ 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve calculates the inverse matrix of 'x' or if this inverse has been previously cached,  
## returns the inverse from cache

cacheSolve <- function(x, ...) {
        ## Retreval of inverse value from 'x'
        inverse <- x$getsolve()
        ## Logical test to see if inverse of 'x' has been calculated previously, 
        ## i.e. is non-null
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        ## Calaculatiion of the inverse
        inverse <- solve(data, ...)
        ## setsolve functon in 'x' acquires the value of the inverse
        x$setsolve(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}




