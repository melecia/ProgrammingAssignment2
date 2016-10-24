## The functions are to make a square invertible matrix, return the inverse and cache it.

## The below is making a square matrix 
## that returns a list that contains the below functions
    ##	1. set the matrix
    ##	2. get the matrix
    ##	3. set the inverse
    ##	4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}

## Using the list from the above makeCacheMatrix as input to the cacheSolve() 
## Then it checks if the inverse already exists and if so then it'll return the cached data
## If it doens't already exists then it'll set the inverse then will return the inverse data

cacheSolve <- function(x, ...) {

  inv = x$getinv()

  if (!is.null(inv)){ 
    message("cached data already exists, here's the cached data")
    return(inv)
  }
  
  cal_hold = x$get()
  inv = solve(cal_hold, ...)
  
  x$setinv(inv)
  
  return(inv)
}
