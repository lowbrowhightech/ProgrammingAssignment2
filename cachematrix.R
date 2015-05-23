## This function creates a special object to receive a square matrix and can then cache the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {

  trueinverse <- NULL
  ## Following internal function changes value of matrix received by the overall function, if callsed to do so.
  changematrixstored <- function(y) {
    x <<-y
    trueinverse <<- NULL
  }
  
  getmatrixstored <- function() x  ## Function that stores the matrix received by the overall function.
  setinverse <- function(inversematrix) trueinverse <<- inversematrix ## Function that establishes inverse.
  getinverse <- function() trueinverse ## Function that retrieves inverse.
  
  ## Stores functions to make them accessible.
  list(changematrixstored = changematrixstored, getmatrixstored = getmatrixstored,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function tests if the inverse of the square matrix is already stored, and then creates the inverse if needed.

cacheSolve <- function(x, ...) {

trueinverse <- x$getinverse()
## An if statement to test if the inverse of the square matrix already exists. If it does, the if statement returns it.
  if(!is.null(trueinverse)) {
    message("getting cached data")
    return(trueinverse)
  }
  ## Following statements compute the inverse of the square matrix in case nothing exists in the variable 'trueinverse'.
  data <- x$getmatrixstored()
  trueinverse <- solve(data)
  x$setinverse(trueinverse)
  trueinverse
}
## Geez, I can't stand doing comments....
