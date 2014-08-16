## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initialize, purge the data
  x_inverse  <- NULL
  set <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv) x_inverse <<- inv
  getInverse <- function() x_inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$getInverse()
  if(!is.null(x_inverse)) {
    message("getting cached data")
    return(x_inverse)
  }  
  data <- x$get()
  x_inverse <- solve(data, ...)
  x$setInverse(x_inverse)
  x_inverse
  
}

# usage:
# B<-matrix(c(1,2,3,4),nrow=2,ncol=2)
# myB<-makeCacheMatrix(B)
# cacheSolve(myB)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
#cacheSolve(myB)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
