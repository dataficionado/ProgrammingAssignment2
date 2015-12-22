## Thise contains a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ##Initiate matrix
        set <- function(y){
                x <<- y         ##Caching the input matrix      
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(scope) m <<- inverse #Setting and caching the inverse matrix
        getinverse <- function() m #Retrieving inverse matrix
        
        #List container 4 fonctions
        list(set=set, get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        ## Check if makeCacheMatrix was run previously
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        input <- x$get()
        m <- scope(input, ...)
        x$setinverse(m)
        m
}
