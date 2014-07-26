## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A Special matrix that caches its inverse

makeCacheMatrix <- function(m = matrix()) {
        ## Initialized inverse i
        i <- NULL
        
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        
        get <- function() m
        
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
        setInverse = setInverse
        getInverse = getInverse
}


## Write a short comment describing this function
## Compute Inverse of the special matrix returned by makeCacheMatrix as above. 
## If inverse is already calculated, then retreive from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
        m<- solve(data)
        
        x$setInverse(m)
        m
}
