## Put comments here that give an overall description of what your
## functions do
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## initialize inv variable to NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <-function(inverse){
        inv <<- inverse
    } 
    getInverse <- function() {
        inv
    }
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve will determine if the inverse of a matrix exists and if 
## it does return it then create it and store it
cacheSolve <- function(x, ...) {
    
    ## create inv to hold the value of the current inverse
    ## this will be NULL if one does not exist
    inv <- x$getInverse ()
    
    ## test to see if inv is not NULL and if an inverse exists return that
    if(!is.null(inv)) {
        message("getting Cached data")
        return(inv)
    }
    
    ## get the matrix we want to find the inverse of
    data <- x$get()
    
    ## use the solve function on the inv to return the inverse
    inv <- solve(data, ...)
    
    ## set the value based on the returned value from solve
    x$setInverse(inv)
    
    ## return back the inverse
    inv
}
