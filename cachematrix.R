# Programming Assignment 2

# The goal of the assignment is to to write a pair of functions that cache the inverse of a matrix.
# Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a 
# matrix rather than compute it repeatedly.
                                          

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# It is a list of functions to
# 1.  set the value of elements of a matrix
# 2.  get the value of elements of the matrix
# 3.  set the value of the inverse of the matrix
# 4.  get the value of the inverse of the matrix

makeCacheMatrix <- function(x=matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(solve)  M <<- solve
  getinverse <- function() M
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}


# The cacheSolve function obtains the inverse of the special "matrix" created with the above makeCacheMatrix function. 
# However, it first checks to see if the inverse matrix has already been calculated. If so, it gets that inverse matrix from the 
# cache and skips the computation. Otherwise, it obtains the inverse matrix of the special "matrix" and sets the value of the 
# elements in the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x,...) {
  M <- x$getinverse()
  if(!is.null(M)) {
    message("getting chached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data,...)
  x$setinverse(M)
  M
}
