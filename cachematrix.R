
## This pair of functions cache the inverse of a matrix. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
        inverse <- NULL
        
        set <- function(x) {
                
                mat <<- x;
                
                inverse <<- NULL;
        }
        
        get <- function() mat;
        
        setinv <- function(inv) inverse <<- inv;
        
        getinv <- function() inverse;
        
        list(set = set, get = get, 
             setinv = setinv, 
             getinv = getinv)
}

## This function computes the inverse of the special "matrix" created with the 'makeCacheMatrix'
## function above. It first checks to see if the inverse has already been computed. 
## If so, it gets the inverse from the setinv function.

cacheSolve <- function(mat, ...) {
        
        inverse <- mat$getinv()
        
        if(!is.null(inverse)) {
                message("Getting cached data...")
                inverse
        }
        
        data <- mat$get()
        
        inverse <- solve(data, ...)
        
        mat$setinv(inverse)
        
        inverse ## Return a matrix that is the inverse of 'x'
}

