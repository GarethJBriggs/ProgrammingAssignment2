## To cache an inverse matrix, makecacheMatrix takes a matrix and creates a matrix object 'x', 
## initiates and sets the inverse value to null and encodes a subsettable list of functions.    
## The functions are getters and setters of 'x' and it's inverse, facilitating the retreval and 
## manipulation of these objects.The updating of 'x' and inverse object values within makecacheMatrix  
## occurs through the use of lexical scoping. With the setters, respectively, resetting the  
## values of 'x'and the inverse, in the parent domain via lexical scoping. This uses the deep assignment 
## operator.Enabling the caching of the inverse and an ability to reset 'x'


## cacheSove returns the inverse matrix of the of 'x' where is 'x' = makecacheMatrix(x). Firstly,
## cacheSolve retreves the inverse value from 'x' and checks if the inverse has been calculated, i.e.
## if the value in non-null. If so, a retreval message is returned and the inverse is returned 
## from cache. If 'x' is null, the inverse is then calculated. Following this, the setslove function
## is called, which uses the newly calulated inverse to set the inverse value in makecacheMatrix.
## The inverse of 'x' is then returned

## makecaheMatrix funtion creates a matrix object 'x' as well as getters and setters to  
## cache the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        ##Set funtion enables the resetting of both the inverse to null in the parent
        ##and of  of 'x' matrix value to 'y', in the parent environment via the use of
        ## the deep assighnment operator
        set <- function (y) {
                x <<- y
                inverse <<- NULL
        }
        ## get funtion acquires value of 'x' from the parent environment
        get <- function() x
        ## setsolve funtion sets the value of the inverse within the parent environment  
        ## via thedeep assighnment operator, from the value of solve 
        setsolve <- function(solve) inverse <<- solve
        ## getsolve funtion acquires the value of the inverse from the parent 
        ## evironment
        getsolve <- function() inverse
        ## functions put into a list as named elements so the getters and setters can 
        ## be accessed viasubsetting using $ 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cachsolve calculates the inverse matrix of 'x' or if this inverse has been previously cached,  
## returns the inverse matrix from cache

cacheSolve <- function(x, ...) {
        ## retreval of inverse value from 'x'
        inverse <- x$getsolve()
        ## logical test to see if inverse of 'x' has been calculated previously, 
        ## i.e. is non-null
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        ## Calaculatiion of the martix inverse
        inverse <- solve(data, ...)
        ## Setsolve functon in 'x' acquires the value of the inverse
        x$setsolve(inverse)
        ## Return a matrix that is the inverse of 'x'
        inverse
}




