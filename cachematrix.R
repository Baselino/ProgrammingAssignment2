## Put comments here that give an overall description of what your
## functions do

## The two functions makeCacheMatrix and cacheSolve can be
## combined to cache the inverse of a matrix. The functions 
## assume that the matrix is invertible. 

## Write a short comment describing this function

## The function makeCacheMatrix takes a matrix object as input. 
## It creates a list object with four list elements. 
## Each list element is a function. 
## The list is the basis for caching the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
  # in global environment inv <- NULL  
    inv <- NULL
  # function set assigns values in parent environment
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
  # function get: calls x (the matrix)
      get <- function() x
  # function setinv: set inverse in parent environment
      setinv <- function(inverse) inv <<- inverse
  # function getinv: call inv
      getinv <- function() inv
  # creating list object as output of this function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve returns a matrix that is the inverse of `x`
## cacheSolve takes as input the list object created by
## makeCacheMatrix and either returns the cached inverse or
## computes and caches the inverse of a matrix

cacheSolve <- function(x, ...) {
  ## call inv from list 
  inv <- x$getinv()
  # if inv is NOT NULL, the cached data is returned
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if inv is NULL, the data from the matrix is obtained
  data <- x$get()
  # the inverse is caclualted with the solve() function
  inv <- solve(data, ...)
  # this is the step where the result is cached
  x$setinv(inv)
  # the inverse is returned as ouptut
  inv
}


### sample code to see it works
# matrix2 <- matrix(c(1, 3, 5, 2, 7, 9, 15, 3, 2), ncol = 3)
# solution <- solve(matrix2)

# cache <- makeCacheMatrix(matrix2)
# result <- cacheSolve(cache)
# result
# identical(result, solution)

