## makeCacheMatrix creates a special "Matrix", containing functions that
## Set and Get the value of the matrix,  Set and Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
   i<- NULL
    
    ## Set the value of the Matrix
    set <- function(y) {
        x <<- y
        i<<- NULL
    }

    ## Get the value of the Matrix
    get <- function() x
    
    # Set the value of the Inverse
    setInverse <- function(inverse) i <<- inverse
    
    # Get the value of the Inverse
    getInverse <- function() i
    
    ## return the Matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve calculates the inverse of the Matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
   i<- x$getInverse()
    
    ## checks to see if the Inverse has already been calculated. 
    ## If so, it gets the Inverse from the cache and skips the computation
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Calculates the Inverse of the matrix 
    matrixx <- x$get()
    i <- solve(matrixx)
    
    ##sets (chche) the value of the Inverse
    x$setInverse(i)
    
    ## Returns the Inverse of the Matrix
    i
}

