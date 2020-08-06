## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## What it done is basically like what is told in the instruction
## set the value of the matrix and then get the value of the matrix
## set the value of inverse and then get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { 
  
  inv <- NULL 
  set <- function(y) {   
  x <<- y    ## value of matrix in parent environment
  inv <<- NULL  
  }
  get <- function() x  
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## then the cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data") 
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) 
  x$setinverse(inv)
  inv
}

