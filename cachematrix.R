## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special object to receive a matrix and can then cache the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  trueinverse <- NULL
  changematrixstored <- function(y) {
    x <<-y
    trueinverse <<- NULL
  }
  getmatrixstored <- function() x
  setinverse <- function(inversematrix) trueinverse <<- inversematrix
  getinverse <- function() trueinverse
  list(changematrixstored = changematrixstored, getmatrixstored = getmatrixstored,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function tests if the inverse matrix is already stored, and then creates the inverse if necessary.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
trueinverse <- x$getinverse()
  if(!is.null(trueinverse)) {
    message("getting cached data")
    return(trueinverse)
  }
  data <- x$getmatrixstored()
  trueinverse <- solve(data)
  x$setinverse(trueinverse)
  trueinverse
}
