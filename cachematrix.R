## The goal is to write two functions that enable us to cache the inverse of a matix
## The use of a cache will enable faster computing since inverting a matrix takes lots of space/memory 

## The makeCacheMatrix is a function that creates a special matrix (from a square matrix) that can cache its inverse for further input

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvert <- function(solve) m <<- solve
  getInvert <- function() m
  list(set = set, get = get,
       setInvert = setInvert,
       getInvert = getInvert)
}


## The cacheSolve is a function that computes the inverse of the matrix from the makeCacheMatrix command above
## Goal is to return a matrix that is the inverse of 'x' by using the solve command

cacheSolve <- function(x, ...) {
  m <- x$getInvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvert(m)
  m
}

## Testing the result
mat <- matrix(1:4, 2, 2)
mat1 <- makeCacheMatrix(mat)
mat ## Checking the value
cacheSolve(mat1)

solve(matrix(1:4,2,2)) ## Gives the same result as that above

## Additional test
mat2 <- matrix(rnorm(4),2,2)
mat3 <- makeCacheMatrix(mat2)
cacheSolve(mat3)
