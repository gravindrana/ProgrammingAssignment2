## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix
## This function creates a special "matrix" object
##      that can cache its inverse
## Input: A matrix whose inverse needs to be computed
## Output: A list that has the following function components
##      set, get, setinverse, getinverse

## Function cacheSolve
## This function computes the inverse 
##      for a square invertible matrix
## Input: A special "matrix" object
##      (previously returned by makeCacheMatrix)
## Output: Inverse of the associated matrix

## Write a short comment describing this function
## Detailed description for makeCacheMatrix
## Defines the 4 function components
## "get" retrieves the associated matrix
## "set" initializes the associated matrix using <<-
##      The inverse should also be iniialized
## "getinverse" retrieves the inverse
## "setinverse" initializes the inverse using <<-
## create a list with the above 4 function components
##      Return the above list

makeCacheMatrix <- function(x = matrix()) {
        ## Assumptions made:
        ##      A square matrix is passed in
        ##      the matrix is always invertible
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, 
                setinverse = setinverse,
                getinverse = getinverse)
}


## Write a short comment describing this function
## Return inverse from cache, if previously calculated
##      and matrix has NOT changed
##      Otherwise, Compute inverse and return
## Prints a message that indicates whether cache is used

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Assumptions made:
        ##      A special "matrix" object is passed in as an argument
        ##      The associated matrix is always invertible
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
