# Function (makeCacheMatrix) creates a special "matrix" object that can
# cache the inverse, a List containing functions to:
# 1.set the value of the Matrix
# 2.get the value of the Matrix
# 3.set the value of the Inverse
# 4.get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Function (cacheSolve) computes the inverse of the special "matrix" 
# returned by (makeCacheMatrix) function On 2 conditions: 
# 1. Inverse has already been calculated  
# 2. matrix has not changed
# then (cachesolve) should retrieve the inverse from the cache
# where (x) is the Special matrix object (Cache object list)
# and (y) is the matrix we need to calculate the inverse for.

cacheSolve<- function(x = list(), y = matrix(), ...) {
        original <- x$get()
        
        # Checking that matrix hasn't changed
        if (all(original == y)){ 
                inv <- x$getinverse()
                
                # Checking that Inverse has already been calculated
                if(!is.null(inv)) {
                        message("getting cached data")
                        return(inv)
                }   
                
                # Otheriwse, calculate Inverse & cache it
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv
        }
        
        # Matrix is different, calculate & return inverse directly
        else {
                inv <- solve(y, ...)
                inv
        }
}
