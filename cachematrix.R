## makeCacheMatrix will create an empty matrix to store the calculated value and also gives function inverse


makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) invrs <<- solveMatrix
  getInverse <- function() invrs
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}




## cacheSolve will calculate the inverse of a matrix and returns the inverse matrix at the end if it was calcuated before

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInverse()
  if(!is.null(invrs)){
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data)
  x$setInverse(invrs)
  invrs
}
