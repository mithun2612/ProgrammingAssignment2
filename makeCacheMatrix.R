## Put comments here that give an overall description of what your
## functions do
##makeCacheMatrix functions creates matrix that cache its inverse
## Write a short comment describing this function
##this function creates a matrix which set the value of the matrix,get value of the matrix,set its inverse and get its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function
##cacheSolve function calcute the inverse of the above matrix,it first check  to see if the inverse has already been calculated if so it get value of the matrix 
##and skip computation otherwise it calculate the inverse of the matrix and set the inerse of the matrix via setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv

  
}
