## The following two functions will both cache and compute the inverse of 
## a matrix


## This function will store a matrix, and allow for:
## #1 Setting the value of the matrix
## #2 Getting the value of the matrix
## #3 Setting the value of the inverse of the matrix
## #4 Getting the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) i <<- inverse
        
        
        getinverse <- function() {
                i
        }
        
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function will calculate the inverse of a matrix.  If the inverse is
## already calculated, then it will simply return the stored inverse calculation

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
    
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        
        i <- solve(data)
        
        x$setinverse(i)
        
        i
}
