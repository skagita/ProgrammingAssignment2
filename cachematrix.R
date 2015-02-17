## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.

## The first function, 'makeCacheMatrix' creates a special "matrix" object,
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(matinverse) inv <<- matinverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function, 'cacheSolve' computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 

cacheSolve<- function(x, ...) {
 
  ## Retrieve the inverse of matrix 'x' from cache by calling a function 
  ## in makeCacheMatrix.
  inv <- x$getinverse()
  
  ## If the inverse has already been calculated (and the matrix has not changed), 
  ## then the cachesolve retrieves the inverse from the cache. The function also 
  ## prints out a confirmation message whenever inverse is retrieved from Cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If cache is empty , calculate the inverse of matrix 'x' using 'solve()',
  ## a built-in R function, and 
  
  data <- x$get()
  inv <- solve(data)
  
  ## Store the inverse in the cache by calling a function in makeCacheMatrix.
  x$setinverse(inv)
  inv
}
