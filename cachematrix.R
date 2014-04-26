## The following functions calculate, cache and return the inverse of a matrix 
## If the the contents of a matrix are not changing, the inverse is retrieved
## from the cache instead of time-consuming repeat calculation


## The function makeCacheMatrix creates a special vector which is a list 
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  ##initialize the value of the inverse of the matrix
  i <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    ##use the scoping rules of R to the set the value of the matrix
    x <<- y
    i <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## get the inverse of the matrix
  getinverse <- function() i
  
  ## return the list containing all the functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The fuction "cacheSolve" retrieves the inverse of the matrix, if it is 
## already cached. If not, it calculates the inverse, caches it and returns 
## the value

cacheSolve <- function(x, ...) {
    
    ## Call the function getinverse to get the inverse of the matrix
    ## from cache
    i <- x$getinverse()
    
    ## If it exists in the cache return the value
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    
    ## If it does not exist in the cache, calculate the inverse
    data <- x$get()
    i <- solve(data, ...)
    
    ## Cache the inverse
    x$setinverse(i)
    
    ## Return the inverse
    i
}
