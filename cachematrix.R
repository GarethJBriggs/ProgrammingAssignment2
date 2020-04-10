## To cache an inverse matrix, makeCacheMatrix takes a matrix and creates a matrix object 'x', 
## initiates and sets the inverse value to null and encodes a subsettable list of functions.    
## The functions are getters and setters of 'x' and it's inverse, facilitating the retreval and 
## manipulation of these objects.The updating of 'x' and inverse object values within  
## makeCacheMatrix occurs through the use of lexical scoping. The setters, respectively, reset the  
## values of 'x'and the inverse in the parent domain via lexical scoping. This uses the deep  
## assignment operator.Enabling the caching of the inverse and an ability to reset 'x'. The encoding
## of the getters and setters within mackecacheMAtrix as named list elements enables these functions
## to be accessable via subscripting, for use in cacheSolve.


## cacheSolve returns the inverse matrix of the of 'x', where is 'x' an argument of makeCacheMatrix().
## Firstly, cacheSolve retreves the inverse value from 'x' and checks if the inverse has been 
## calculated, i.e.if the value is non-null. If so, a retreval message is displyed and the inverse  
## is returned from cache. If the inverse is null, it is then calculated. Following this, the 
## setslove function is called, which uses the newly calulated inverse to set the inverse value in 'x'.
## The inverse of 'x' is then returned

## makeCacheMatrix funtion creates a matrix object 'x' as well as getters and setters to  
## cache the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        ##set funtion enables the resetting of both the inverse to null and of 'x' to the value
        ## of 'y', in the parent environment via the use of the deep assighnment operator
        set <- function (y) {
                x <<- y
                inverse <<- NULL
        }
        ## get funtion acquires value of 'x' from the parent environment
        get <- function() x
        ## setsolve funtion sets the value of the inverse within the parent environment  
        ## via thedeep assighnment operator, from the value of solve 
        setsolve <- function(solve) inverse <<- solve
        ## getsolve funtion acquires the value of the inverse from the parent evironment
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
        ## Calaculatiion of the  inverse
        inverse <- solve(data, ...)
        ## setsolve functon in 'x' acquires the value of the inverse
        x$setsolve(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}




