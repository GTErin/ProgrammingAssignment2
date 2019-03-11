## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the matrix inverse as null
    inverse <- NULL
    ## Define the set function of the matrix
    set <- function(y){
    x <<- y
    ## Reset the inverse to NULL if there is a new matrix
    inverse <<- NULL
  }
  ## get function returns the value of the matrix
  get <- function() x
  ## Set the value of the inverse matrix
  setinverse <- function(i) inverse <<- i
  ## Get the value of the inverse matrix
  getinverse <- function() inverse
  ##required to refer to functions as needed
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  }
  
  ## Write a short comment describing this function
  ## cacheSolve takes the output of makeCacheMatrix and calculates the inverse
  ## If the inverse has already been calculated then cacheSolve retrieves the inverse and skips the calculation
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    ## If the matrix is not NULL 
    if(!is.null(inverse)){
      ## Return the message
      message("Getting cached data")
      ## Then return the inverse matrix
      return(inverse)
    }
    ##Otherwise, calculate the inverse matrix and return it
    data <- x$get()
    inverse <- solve(data, ...)
    x$getinverse(inverse)
    inverse
  }
