## Caching the Inverse of a Matrix
## This file contains a pair of functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
  # Values of the inverse matrix
  inv <- NULL
  
  # The function sets values of the matrix
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  # The function returns values of the matrix
  get <- function()
  {
    x
  }
  
  # The function stores values of the inverse matrix
  setinverse <- function(matrix.inv)
  {
    inv <<- matrix.inv
  }
  
  # The function returns the inverse matrix
  getinverse <- function()
  {
    inv
  }
  
  # This list represents a special "matrix" object that can cache its inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...)
{
  
  inv <- x$getinverse()
  ## Verification if we have already  the inverse matrix in our cache
  if(!is.null(inv))
  {
    message("Getting cached data...")
    return(inv)
  }
  
  # Let's obtain values of the matrix 'x'
  data <- x$get()
  
  # Let's calculate the inverse matrix...
  inv <- solve(data, ...)
  # and store values
  x$setinverse(inv)
  
  ## Let's return a matrix that is the inverse of 'x'
  inv
}
