## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix defines a list of 4 functions used to;
## set the value of the matrix.
## get the value of the matrix.
## set the value of inverse of the matrix.
## get the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

## using the function provided in makeCacheMatrix, cacheSolve
## first determines if the inverse has already been calculated.
## If this value "is Null" i.e. non existant in the environment, it proceeds to use the
## solve function to find the inverse. 
## The first iteration through cacheSolve will calculate the inverse and show "getting
## cached data". Subsequent iterations will used this cached version and return it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
