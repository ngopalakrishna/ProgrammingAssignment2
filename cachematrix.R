## makeCacheMatrix creates a matrix or sets/gets the inverse.
## cacheSolve returns calculated inverse or inverse from cache if exists.

## Creates list of functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of matrix
## 4. get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) inverse <<- solve
        
        getinverse <- function() inverse
        
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## Function returns the inverse of the matrix
## If its already calculated, it returns the inverse
## from the cached value. Else calculates the inverse
## and stores the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
              message("Returning cached data")
              return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data)
        
        x$setinverse(inverse)
        inverse
}
