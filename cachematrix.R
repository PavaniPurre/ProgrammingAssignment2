## makeCacheMatrix: This function creates a special "matrix" object that can cache 
# its inverse.
# The object does not calculate the inverse, just saves it inside.
# Saves the matrix to variable x and its inverse to variable inverse in scope.
# Returned object (actually it's a list) contains methods:
# set: sets matrix and resets cached inverse
# get: returns matrix
# setinverse: saves solve value
# getinverse: returns cached inverse value

makeCacheMatrix <- function(x = matrix()) 
{
  # initialize the matrix to NULL
  inverse  <- NULL
  set <- function(y) 
  {
    x <<- y
    inverse <<- NULL
  }
  get <- function() 
  {
    x
  }
  
  setinverse <- function(inv)
  { 
    inverse <<- inv
  }
  getinverse <- function()
  {
    inverse
  } 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) 
  {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
